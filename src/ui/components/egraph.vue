<template>
    <div class="egraph--container">
        <graphviz-svg ref="gv" class="egraph" :graph="_graph"
            :layoutStylesheet="layoutStylesheet"
            @rendered="colorOverlay"
            @mousedown="onMouseDown"
            @mouseover="onMouseOver"/>
        <div class="egraph--stats">{{stats}}<br/>{{hover}}</div>
    </div>
</template>

<style>
@import "./egraph.css";  /** @todo kremlin drops the ball here */
</style>


<script lang="ts">
import _ from 'lodash';
import { Component, Vue } from 'vue-facing-decorator'

// @ts-ignore
import GraphvizSvg from './graphviz-svg.vue';
import './egraph.css';
import { ColorEGraphOverlay } from '../graphs/viz-colors';

/*
@Component({
    //components: { GraphvizSvg }
})
export default class Egraph extends Vue {
    //name: string = "hello"
}
*/

export default {
    props: ['egraph', 'format', 'layoutStylesheet', 'overlay'],
    data: () => ({
        size: {x: 150, y: 150},
        hover: {node: undefined, edge: undefined}
    }),
    
    computed: {
        _graph() { return this.egraph?.toGraph(this.format); },
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
            if (this.overlay) {
                let co = new ColorEGraphOverlay(this.egraph, rendered);
                co.apply();
                co.on('eclass:select', ev => this.$emit('eclass:select', ev));
                this._overlay = co;
            }
        },
        onMouseDown(ev) {
            let node = this.$refs.gv.nodeFromElement(ev.target);
            let eclass = this._overlay?.eclassFromElement(ev.target);
            console.log(node, eclass)
            if (node || eclass)
                this.$emit('select', {node, eclass})
        },
        onMouseOver(ev) {
            this.hover.node = this.$refs.gv.nodeFromElement(ev.target)
                              ?? this._overlay?.eclassFromElement(ev.target);
        }
    },
    components: { GraphvizSvg }
}
</script>