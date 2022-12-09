import 'dart:io';

class Grid {
  var _grid;
  int _width = 0;
  int _height = 0;

  Grid(grid, width, height) {
    this._grid = grid;
    this._width = width;
    this._height = height;
  }
  int get(i, j) { return _grid[j*_height+i]; }

  bool isVisible(i, j) {
    var targetHeight = get(i, j);
    
    bool canSee(from, to, getter) {
      for (var a=from; a<to; ++a) {
        var h = getter(a);
        if (h >= targetHeight) {
          return false;
        }
      }
      return true;
    }
    
    if (canSee(0, i, (x){return get(x,j);})) return true;
    if (canSee(i+1, _width, (x){return get(x,j);})) return true;
    if (canSee(0, j, (y){return get(i,y);})) return true;
    if (canSee(j+1, _height, (y){return get(i,y);})) return true;
    return false;
  }

  int getScore(i, j) {
    var targetHeight = get(i,j);
    int countVisible(from, to, getter) {
      var visibleCount = 0;
      var dir = to > from ? 1 : -1;
      var count = (to - from).abs();
      for (var delta=0; delta<count; ++delta) {
        ++visibleCount;
        var h = getter(from + delta*dir);
        if (h >= targetHeight) {
          break;
        }
      }
      return visibleCount;
    }
    return countVisible(i-1,-1,(x){return get(x,j);})
    * countVisible(i+1,_width,(x){return get(x,j);})
    * countVisible(j-1,-1,(y){return get(i,y);})
    * countVisible(j+1,_height,(y){return get(i,y);});
  }
}

void main() {
  File('day8.input').readAsString().then((String data) {
      var lines = data.split("\n");
      var width = lines[0].length;
      var height = lines.length;
      var gridData = <int>[];
      for (var line in lines) {
        for (var lineIndex=0; lineIndex<line.length; ++lineIndex) {
          gridData.add(int.parse(line[lineIndex]));
        }
      }
      var grid = Grid(gridData, width, height);

      var visibleCount = 2*width + 2*height - 4;
      for (var i=1; i<width-1; ++i){
        for (var j=1; j<height-1; ++j) {
          if (grid.isVisible(i, j)) {
            ++visibleCount;
          }
        }
      }
      print("part1: $visibleCount");

      var maxScore = 0;
      for (var i=1; i<width-1; ++i) {
        for (var j=1; j<height-1; ++j) {
          var score = grid.getScore(i, j);
          if (score > maxScore) {
            maxScore = score;
          }
        }
      }
      print("part2: $maxScore");
  });
}
