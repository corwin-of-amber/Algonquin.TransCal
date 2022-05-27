<template>
    <svg ref="drawGraph" class="graphviz" :height="size.y" :width="size.x">
    </svg>
</template>

<script>
import { GraphvizAdapter } from '../graphs/graphviz';

export default {
    props: ['graph', 'layoutStylesheet'],
    data: () => ({
        size: {x: 150, y: 150},
    }),
    mounted() {
        this.adapter = new GraphvizAdapter();
    },
    watch: {
        async graph() {
            this.adapter.stylesheet = this.layoutStylesheet;
            let svg = await this.adapter.render(this.graph);
            this.$refs.drawGraph.textContent = '';
            this.$refs.drawGraph.append(...svg.children);
            this.size = {
                x: svg.width.baseVal.valueInSpecifiedUnits,
                y: svg.height.baseVal.valueInSpecifiedUnits
            };
        }
    },
    methods: {
        nodeFromElement(el) {
            var node = el.closest('.node');
            return node ? this.graph.node(node.id) : undefined;
        }
    }
}
</script>
