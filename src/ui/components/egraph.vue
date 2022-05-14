<template>
    <div class="egraph--container">
        <svg ref="drawGraph" :height="size.y" :width="size.x">
        </svg>
        <div class="egraph--stats">{{stats}}</div>
    </div>
</template>

<script>
import { GraphvizAdapter } from '../graphs/graphviz';

export default {
    props: ['egraph'],
    data: () => ({size: {x: 150, y: 150}}),
    mounted() {
        this.adapter = new GraphvizAdapter();
    },
    watch: {
        async egraph() {
            let svg = await this.adapter.render(this.egraph);
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
    }
}
</script>

<style>

</style>