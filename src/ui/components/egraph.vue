<template>
    <div class="egraph--container">
        <graphviz-svg ref="gv" class="egraph" :graph="_graph"
            :layoutStylesheet="layoutStylesheet"
            @rendered="colorOverlay"
            @mouseover="onMouseOver"/>
        <div class="egraph--stats">{{stats}}<br/>{{hover}}</div>
    </div>
</template>

<style>
@import "./egraph.css";  /** @todo kremlin drops the ball here */
</style>


<script>
import _ from 'lodash';
import GraphvizSvg from './graphviz-svg.vue';
import './egraph.css';
import { ColorEGraphOverlay } from '../graphs/viz-colors';

export default {
    props: ['egraph', 'layoutStylesheet'],
    data: () => ({
        size: {x: 150, y: 150},
        hover: {node: undefined, edge: undefined}
    }),
    
    computed: {
        _graph() { return this.egraph?.toGraph(); },
        stats() {
            var g = this.egraph;
            return g && `${g.nodeCount?.()} nodes, ${g.edges.length} hyperedges`;
        }
    },
    methods: {
        colorOverlay(rendered) {
            if (rendered.graph !== this._graph) return;

            let p = this.egraph.colors?.palette, c;
            for (let {el, node} of rendered.iterNodeElements()) {
                if ((c = node.data?.color) !== undefined) {
                    el.classList.add('hypernode--colored');
                    el.setAttribute('data-color', p.get(c).name ?? c);
                }
            }
            // Display colored merges
            let co = new ColorEGraphOverlay(this.egraph, rendered);
            for (let c of this.egraph.colors?.eclasses ?? []) {
                co.connectNodes(c.members);
            }
        },
        onMouseOver(ev) {
            this.hover.node = this.$refs.gv.nodeFromElement(ev.target);
        }
    },
    components: { GraphvizSvg }
}

</script>