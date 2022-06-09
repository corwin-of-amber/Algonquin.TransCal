<template>
    <div>
        <div class="toolbar">
            <div class="toolbar--switch">
                <span>Sam</span><slider-switch :value="mode == 'max'" @input="mode = $event ? 'max' : 'sam'"/><span>Max</span>
            </div>
        </div>
        <egraph :egraph="egraph"
            :format="config.format"
            :overlay="config.overlay"
            :layoutStylesheet="config.layoutStylesheet"/>
    </div>
</template>

<style>
.toolbar--switch > span {
    margin: 0 0.2em;
    font-family: Arial, Helvetica, sans-serif;
    font-size: 80%;
}
</style>

<script>
import EGraphComponent from './egraph.vue';
import SliderSwitch from './widgets/slider-switch.vue';
import { EGraph } from '../graphs/egraph';
import { Hypergraph } from '../graphs/hypergraph';


export default {
    props: ['egraph'],
    data: () => ({mode: 'max'}),
    computed: {
        config() {
            switch (this.mode) {
            case 'sam':
                return {
                    format: new Hypergraph.GraphFormatting,
                    layoutStylesheet: {
                       graph: {rankdir: "BT", ranksep: 0.3, nodesep: 0.4, compound: true}
                    },
                    overlay: true
                };
            case 'max':
                return {
                    format: new EGraph.ClusteredFormatting,
                    layoutStylesheet: {
                        graph: {rankdir: "BT", ranksep: 1.2, nodesep: 0.4}
                    },
                    overlay: false
                };
            }
        }
    },
    methods: {
        onSwitch(ev) {
            this.mode = ev? 'max' : 'sam'            ;
            console.log(ev);
        }
    },
    components: { 'egraph': EGraphComponent, SliderSwitch }
}
</script>