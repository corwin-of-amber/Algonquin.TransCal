<template>
    <div class="egraph--container">
        <graphviz-svg ref="gv" class="egraph" :graph="egraph"
            :layoutStylesheet="layoutStylesheet"
            @mouseover="onMouseOver"/>
        <div class="egraph--stats">{{stats}}<br/>{{hover}}</div>
    </div>
</template>

<style>
@import "./egraph.css";  /** @todo kremlin drops the ball here */
</style>


<script>
import { GraphvizAdapter } from '../graphs/graphviz';
import GraphvizSvg from './graphviz-svg.vue';
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
    
    computed: {
        stats() {
            var g = this.egraph;
            return g && `${g.nodeCount?.()} nodes, ${g.edgeCount?.()} hyperedges`;
        }
    },
    methods: {
        onMouseOver(ev) {
            this.hover.node = this.$refs.gv.nodeFromElement(ev.target);
        }
    },
    components: { GraphvizSvg }
}
</script>