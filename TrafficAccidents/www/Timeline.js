/* global L */

//import IntervalTree from 'IntervalTree'
/**
 * A node in the interval tree.
 *
 * @property {number} low Start of the interval
 * @property {number} high End of the interval
 * @property {number} min The lowest endpoint of this node's interval or any of
 * its children.
 * @property {number} max The greatest endpoint of this node's interval or any
 * of its children.
 * @property {*} data The value of the interval
 * @property {IntervalTreeNode?} left Left child (lower intervals)
 * @property {IntervalTreeNode?} right Right child (higher intervals)
 * @property {IntervalTreeNode?} parent The parent of this node
 * @private
 */
class IntervalTreeNode {
  constructor(low, high, data, parent) {
    this.low = low;
    this.high = high;
    this.min = low;
    this.max = high;
    this.data = data;
    this.left = null;
    this.right = null;
    this.parent = parent;
  }
}

/**
 * An interval tree is a data structure that holds intervals. For example, if
 * you had events which took place over a period of time, you might store them
 * in an interval tree where the interval is their duration.
 *
 * It allows you to find all intervals which contain a specific point, or
 * overlap with a given interval.
 */
class IntervalTree {
  /**
   * Constructs an empty interval tree.
   */
  constructor() {
    this._root = null;
    /** @type {number} */
    this.size = 0;
  }

  /**
   * Actually insert a new interval into the tree. This has a few extra
   * arguments that don't really need to be exposed in the public API, hence the
   * separation.
   *
   * @private
   * @param {number} begin Start of the interval
   * @param {number} end End of the interval
   * @param {*} value The value of the interval
   * @param {IntervalTreeNode?} node The current place we are looking at to add
   * the interval
   * @param {IntervalTreeNode?} parent The parent of the place we are looking to
   * add the interval
   * @param {string} parentSide The side of the parent we're looking at
   * @returns {IntervalTreeNode} The newly added node
   */
  _insert(begin, end, value, node, parent, parentSide) {
    let newNode;
    if (node === null) {
      // The place we're looking at is available; let's put our node here.
      newNode = new IntervalTreeNode(begin, end, value, parent);
      if (parent === null) {
        // No parent? Must be root.
        this._root = newNode;
      } else {
        // Let the parent know about its new child
        parent[parentSide] = newNode;
      }
    } else {
      // No vacancies. Figure out which side we should be putting our interval,
      // and then recurse.
      const side = (begin < node.low || begin === node.low && end < node.high)
        ? 'left'
        : 'right';
      newNode = this._insert(begin, end, value, node[side], node, side);
      node.max = Math.max(node.max, newNode.max);
      node.min = Math.min(node.min, newNode.min);
    }
    return newNode;
  }

  /**
   * Insert a new value into the tree, for the given interval.
   *
   * @param {number} begin The start of the valid interval
   * @param {number} end The end of the valid interval
   * @param {*} value The value for the interval
   */
  insert(begin, end, value) {
    this._insert(begin, end, value, this._root, this._root);
    this.size++;
  }

  _lookup(point, node = this._root) {
    const overlaps = [];
    if (node === null || node.max < point) {
      return overlaps;
    }
    overlaps.push(...this._lookup(point, node.left));
    if (node.low <= point) {
      if (node.high >= point) {
        overlaps.push(node.data);
      }
      overlaps.push(...this._lookup(point, node.right));
    }
    return overlaps;
  }

  /**
   * Find all intervals that cover a certain point.
   *
   * @param {number} point The sought point
   * @returns {*[]} An array of all values that are valid at the given point.
   */
  lookup(point) {
    return this._lookup(point);
  }

  _overlap(begin, end, node = this._root) {
    const overlaps = [];
    if (!(begin > node.high || node.low > end)) {
      overlaps.push(node.data);
    }
    if (node.left && node.left.max >= begin) {
      overlaps.push(...this._overlap(begin, end, node.left));
    }
    if (node.right && node.right.min <= end) {
      overlaps.push(...this._overlap(begin, end, node.right));
    }
    return overlaps;
  }

  /**
   * Find all intervals that overlap a certain interval.
   *
   * @param {number} begin The start of the valid interval
   * @param {number} end The end of the valid interval
   * @returns {*[]} An array of all values that overlap the given interval.
   */
  overlap(begin, end) {
    return this._overlap(begin, end);
  }
}


L.Timeline = L.GeoJSON.extend({
  times:  null,
  ranges: null,

  /**
   * @constructor
   * @param {Object} geojson The GeoJSON data for this layer
   * @param {Object} options Hash of options
   * @param {Function} [options.getInterval] A function which returns an object
   * with `start` and `end` properties, called for each feature in the GeoJSON
   * data.
   * @param {Boolean} [options.drawOnSetTime=true] Make the layer draw as soon
   * as `setTime` is called. If this is set to false, you will need to call
   * `updateDisplayedLayers()` manually.
   */
  initialize(geojson, options = {}) {
    this.times = [];
    this.ranges = new IntervalTree();
    const defaultOptions = {
      drawOnSetTime: true,
    };
    L.GeoJSON.prototype.initialize.call(this, null, options);
    L.Util.setOptions(this, defaultOptions);
    L.Util.setOptions(this, options);
    if (this.options.getInterval) {
      this._getInterval = (...args) => this.options.getInterval(...args);
    }
    if (geojson) {
      this._process(geojson);
    }
  },

  _getInterval(feature) {
    const hasStart = 'start' in feature.properties;
    const hasEnd = 'end' in feature.properties;
    if (hasStart && hasEnd) {
      return {
        start: new Date(feature.properties.start).getTime(),
        end:   new Date(feature.properties.end).getTime(),
      };
    }
    return false;
  },

  /**
   * Finds the first and last times in the dataset, adds all times into an
   * array, and puts everything into an IntervalTree for quick lookup.
   *
   * @param {Object} data GeoJSON to process
   */
  _process(data) {
    // In case we don't have a manually set start or end time, we need to find
    // the extremes in the data. We can do that while we're inserting everything
    // into the interval tree.
    let start = Infinity;
    let end = -Infinity;
    data.features.forEach((feature) => {
      const interval = this._getInterval(feature);
      if (!interval) { return; }
      this.ranges.insert(interval.start, interval.end, feature);
      this.times.push(interval.start);
      this.times.push(interval.end);
      start = Math.min(start, interval.start);
      end = Math.max(end, interval.end);
    });
    this.start = this.options.start || start;
    this.end = this.options.end || end;
    this.time = this.start;
    if (this.times.length === 0) {
      return;
    }
    // default sort is lexicographic, even for number types. so need to
    // specify sorting function.
    this.times.sort((a, b) => a - b);
    // de-duplicate the times
    this.times = this.times.reduce((newList, x, i) => {
      if (i === 0) {
        return newList;
      }
      const lastTime = newList[newList.length - 1];
      if (lastTime !== x) {
        newList.push(x);
      }
      return newList;
    }, [this.times[0]]);
  },

  /**
   * Sets the time for this layer.
   *
   * @param {Number|String} time The time to set. Usually a number, but if your
   * data is really time-based then you can pass a string (e.g. '2015-01-01')
   * and it will be processed into a number automatically.
   */
  setTime(time) {
    this.time = typeof time === 'number' ? time : new Date(time).getTime();
    if (this.options.drawOnSetTime) {
      this.updateDisplayedLayers();
    }
    this.fire('change');
  },

  /**
   * Update the layer to show only the features that are relevant at the current
   * time. Usually shouldn't need to be called manually, unless you set
   * `drawOnSetTime` to `false`.
   */
  updateDisplayedLayers() {
    // This loop is intended to help optimize things a bit. First, we find all
    // the features that should be displayed at the current time.
    const features = this.ranges.lookup(this.time);
    // Then we try to match each currently displayed layer up to a feature. If
    // we find a match, then we remove it from the feature list. If we don't
    // find a match, then the displayed layer is no longer valid at this time.
    // We should remove it.
    for (let i = 0; i < this.getLayers().length; i++) {
      let found = false;
      const layer = this.getLayers()[i];
      for (let j = 0; j < features.length; j++) {
        if (layer.feature === features[j]) {
          found = true;
          features.splice(j, 1);
          break;
        }
      }
      if (!found) {
        const toRemove = this.getLayers()[i--];
        this.removeLayer(toRemove);
      }
    }
    // Finally, with any features left, they must be new data! We can add them.
    features.forEach(feature => this.addData(feature));
  },
});

L.timeline = (geojson, options) => new L.Timeline(geojson, options);
