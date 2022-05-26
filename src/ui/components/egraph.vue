<template>
    <div class="egraph--container">
        <svg ref="drawGraph" class="egraph" :height="size.y" :width="size.x"
            @mouseover="onMouseOver">
        </svg>
        <div class="egraph--stats">{{stats}}<br/>{{hover}}</div>
    </div>
</template>

<style>
@import "./egraph.css";  /** @todo kremlin drops the ball here */
</style>


<script>
import { GraphvizAdapter } from '../graphs/graphviz';
import './egraph.css';

export default {
    props: ['egraph', 'layoutStylesheet'],
    data: () => ({
        size: {x: 150, y: 150},
        hover: {node: undefined, edge: undefined}
    }),
    mounted() {
        this.adapter = new GraphvizAdapter();
    },
    watch: {
        async egraph() {
            this.adapter.stylesheet = this.layoutStylesheet;
            let svg = await this.adapter.render(this.egraph);
            this.$refs.drawGraph.textContent = '';
            this.$refs.drawGraph.append(...svg.children);
            this.size = {
                x: svg.width.baseVal.valueInSpecifiedUnits,
                y: svg.height.baseVal.valueInSpecifiedUnits
            };
        }
    },
    computed: {
        stats() {
            var g = this.egraph;
            if (g)
                return `${g.nodeCount?.()} nodes, ${g.edgeCount?.()} hyperedges`;
        }
    },
    methods: {
        onMouseOver(ev) {
            var node = ev.target.closest('.node');
            this.hover.node = node ? this.egraph.node(node.id) : undefined;
        }
    }
}
</script>