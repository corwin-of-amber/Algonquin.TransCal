<template>
    <div class="egraph--container">
        <graphviz-svg ref="gv" class="egraph" :class="{'hide-ghost': !showGhost}"
            :graph="_graph"
            :layoutStylesheet="layoutStylesheet"
            @rendered="colorOverlay"
            @mousedown="onMouseDown"
            @mouseover="onMouseOver"/>
        <div class="egraph--stats">{{hover}}<br/>{{stats}}</div>
    </div>
</template>


<script lang="ts">
import { Component, Prop, Vue } from 'vue-facing-decorator'

// @ts-ignore
import GraphvizSvgComponent from './graphviz-svg.vue';
import { ColorEGraphOverlay } from '../graphs/viz-colors';
import type { EGraph } from '../graphs/egraph';
import type { GraphvizSvg } from '../graphs/graphviz';

import "./egraph.css"; /** @todo kremlin does not support <style src=..>? */


@Component({
    emits: ['select'],
    components: { GraphvizSvg: GraphvizSvgComponent }
})
export default class EgraphView extends Vue {
    @Prop egraph: EGraph
    @Prop format: any
    @Prop layoutStylesheet: undefined
    @Prop overlay: undefined

    size = {x: 150, y: 150}
    hover = {node: undefined, edge: undefined}
    showGhost = false

    get _graph() { return this.egraph?.toGraph(this.format); }
    get stats() {
        var g = this.egraph;
        return g && `${g.nodeCount?.()} nodes, ${g.edges.length} hyperedges`;
    }

    onMouseOver(ev: MouseEvent) {
        // @ts-ignore
        this.hover.node = this.$refs.gv.nodeFromElement(ev.target)
                          // @ts-ignore
                          ?? this._overlay?.eclassFromElement(ev.target);
    }

    onMouseDown(ev) {
        let helem = this.hyperelementFromElement(ev.target);
        // @ts-ignore
        let eclass = this._overlay?.eclassFromElement(ev.target);
        if (helem || eclass)
            this.$emit('select', {target: helem, eclass})
    }

    hyperelementFromElement(el: Element) {
        // @ts-ignore
        let node = this.$refs.gv.nodeFromElement(el);
        return {node: (node?.class === 'hypernode') ? node : undefined,
                edge: (node?.class === 'hyperedge--nucleus') ? node.data.edge : undefined};
    }

    colorOverlay(rendered: GraphvizSvg) {
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
            // @ts-ignore
            this._overlay = co;
        }
    }

}
</script>