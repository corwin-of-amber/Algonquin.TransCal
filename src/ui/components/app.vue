<template>
    <div>
        <div class="toolbar">
            <div class="toolbar--switch">
                <span>Sam</span><slider-switch :value="mode == 'max'" @input="mode = $event ? 'max' : 'sam'"/><span>Max</span>
            </div>
        </div>
        <egraph ref="egraph" :egraph="egraph"
            :format="config.format"
            :overlay="config.overlay"
            :layoutStylesheet="config.layoutStylesheet"
            @eclass:select="events.emit('eclass:select', $event)"/>
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
import { EventEmitter } from 'events';
import EGraphComponent from './egraph.vue';
import SliderSwitch from './widgets/slider-switch.vue';
import { EGraph } from '../graphs/egraph';
import { Hypergraph } from '../graphs/hypergraph';


export default {
    props: ['egraph'],
    data: () => ({mode: 'max', override: {ranksep: undefined, nodesep: undefined}}),
    computed: {
        config() {
            let c = this.baseConfig(),
                o = this.override;
            if (o.ranksep !== undefined) c.layoutStylesheet.graph.ranksep = o.ranksep;
            if (o.nodesep !== undefined) c.layoutStylesheet.graph.nodesep = o.nodesep;
            return c;
        }
    },
    created() {
        this.events = new EventEmitter();
    },
    mounted() {
        let cfg = localStorage['config.app'];
        if (cfg) Object.assign(this, JSON.parse(cfg));
        window.addEventListener('beforeunload', () =>
            localStorage['config.app'] = JSON.stringify(this.$data));
    },    
    methods: {
        baseConfig() {
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
    components: { 'egraph': EGraphComponent, SliderSwitch }
}
</script>