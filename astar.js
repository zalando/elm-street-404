"use strict";

const nodeEq   = (a, b) => a.tile[0] == b.tile[0] && a.tile[1] == b.tile[1];
const remove   = (list, node) => list && list.filter(n => !nodeEq(n, node));
const add      = (list, node) => (list || []).concat(node);
const contains = (list, node) => list && list.some(n => nodeEq(n, node));
const toNode   = tile => { return {tile: tile} };
const getNode  = (grid, tile) => grid[tile[0]][tile[1]];

const absDistance = (a, b) => {
    return Math.sqrt(
        Math.pow(a.tile[0] - b.tile[0], 2) +
        Math.pow(a.tile[1] - b.tile[1], 2));
};

const heuristics = (node, destination) => {
    return (
        Math.abs(node.tile[0] - destination.tile[0]) +
        Math.abs(node.tile[1] - destination.tile[1])
    );
};

const createGrid = (gridSize, obstacles) => {
    const grid          = [];
    const obstacleNodes = obstacles.map(toNode);

    for (let x = 0; x < gridSize[0]; x++) {
        grid[x] = grid[x] || [];
        for (let y = 0; y < gridSize[1]; y++) {
            const node    = toNode([x, y])
            node.obstacle = contains(obstacleNodes, node);
            grid[x][y]    = node;
        }
    }

    return grid;
};

const pathToTileList = current => {
    const list = [];
    for (;;) {
        if (!current) {
            return list;
        }

        list.unshift(current.tile);
        current = current.parent;
    }
};

const getNeighbors = (grid, current) => {
    let neighbors;

    const cx = current.tile[0];
    const cy = current.tile[1];

    for (let x = cx - 1; x <= cx + 1; x++)
    for (let y = cy - 1; y <= cy + 1; y++) {
        if (x >= 0 && x < grid.length &&
            y >= 0 && y < grid[0].length &&
            (x != cx || y != cy)) {
            neighbors = add(neighbors, getNode(grid, [x, y]));
        }
    }

    return neighbors;
};

const astar = (gridSize, obstacles, start, destination) => {
    const grid = createGrid(gridSize, obstacles);
    const dest = getNode(grid, destination);

    let openList   = [getNode(grid, start)];
    let closedList = [];

    for (;openList.length;) {
        const current = openList.reduce((previous, current) =>
            (current.f || 0) < (previous.f || 0) ? current : previous);

        if (nodeEq(current, dest)) {
            return pathToTileList(current);
        }

        openList   = remove(openList, current);
        closedList = add(closedList, current);

        getNeighbors(grid, current)
            .filter(neighbor => !contains(closedList, neighbor))
            .filter(neighbor => !neighbor.obstacle)
            .forEach(neighbor => {
            const g = current.g + absDistance(current, neighbor);

            let gBest = false;
            if (!contains(openList, neighbor)) {
                gBest      = true;
                neighbor.h = heuristics(neighbor, dest);
                openList   = add(openList, neighbor);
            } else if (g < neighbor.g) {
                gBest = true;
            }

            if (gBest) {
                neighbor.parent = current;
                neighbor.g      = g;
                neighbor.f      = neighbor.g + neighbor.h;
            }
        });
    }

    return [];
};

const gridSize    = [3, 3];
const obstacles   = [[1, 1]];
const start       = [0, 0];
const destination = [2, 2];
const result      = astar(gridSize, obstacles, start, destination);

console.error(result);
