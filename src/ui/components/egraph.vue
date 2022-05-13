<template>
    <div class="egraph--container">
        <svg ref="drawGraph" :height="height">
        </svg>
        <div class="egraph--stats">{{stats}}</div>
    </div>
</template>

<script>
import { GraphvizAdapter } from '../graphs/graphviz';

export default {
    props: ['egraph'],
    data: () => ({height: 150}),
    mounted() {
        this.adapter = new GraphvizAdapter();
    },
    watch: {
        async egraph() {
            let svg = await this.adapter.render(this.egraph);
            console.log(svg); //.getAttributeNS('viewPort'));
            window.sbg = svg
            this.$refs.drawGraph.append(...svg.children);
            this.height = svg.height.baseVal.valueInSpecifiedUnits;
        }
    },
    computed: {
        stats() {
            var g = this.egraph;
            console.log(g);
            if (g)
                return `${g.nodeCount?.()} nodes, ${g.edgeCount?.()} hyperedges`;
        }
    }
}
</script>

<style>

</style>