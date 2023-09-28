<template>
    <div>
        <div class="toolbar">
            <div class="toolbar--switch">
                <span>Sam</span><slider-switch :value="opts.mode == 'max'" @input="opts.mode = $event ? 'max' : 'sam'"/><span>Max</span>
            </div>
            <div class="toolbar--num">rw depth <numeric-input controls v-model="opts.rwDepth"></numeric-input></div>
        </div>
        <egraph-view ref="egraph" :egraph="egraph"
            :format="config.format"
            :overlay="config.overlay"
            :layoutStylesheet="config.layoutStylesheet"
            @select="this.events.emit('egraph:select', $event)"
            @eclass:select="this.events.emit('eclass:select', $event)"/>
    </div>
</template>

<style>
.toolbar {
    font-family: Arial, Helvetica, sans-serif;
    font-size: 80%;
}
.toolbar > div  { display: inline-block; margin-right: 1em; }
.toolbar--switch > span {
    margin: 0 0.2em;
}
.vue-number-input {
    display: inline-block !important;
    vertical-align: middle;
    font-size: unset !important;
}
.vue-number-input__input {
    font-size: unset !important;
    line-height: unset !important;
    min-height: unset !important;
    min-width: unset !important;
    width: 1.5em !important;
    padding: unset !important;
}
.vue-number-input--controls>input {
    padding-left: 1rem !important;
    padding-right: 1rem !important;
}
.vue-number-input__button {
    width: 1em !important;
}
</style>

<script lang="ts">
import { EventEmitter } from 'events';
import { Component, Vue } from 'vue-facing-decorator'
import NumericInput from '@chenfengyuan/vue-number-input';

// @ts-ignore
import EGraphComponent from './egraph.vue';
// @ts-ignore
import SliderSwitch from './widgets/slider-switch.vue';
import { EGraph } from '../graphs/egraph';
import { Hypergraph } from '../graphs/hypergraph';

@Component({
    components: { NumericInput, SliderSwitch, 'egraph-view': EGraphComponent }
})
export default class App extends Vue {
    egraph: EGraph = undefined
    opts: {
        mode: 'sam' | 'max'
        rwDepth: number
    } = {
        mode: 'sam', rwDepth: 1
    }
    override = {ranksep: undefined, nodesep: undefined}

    events: EventEmitter

    get config() {
        let c = this.baseConfig(),
            o = this.override;
        if (o.ranksep !== undefined) c.layoutStylesheet.graph.ranksep = o.ranksep;
        if (o.nodesep !== undefined) c.layoutStylesheet.graph.nodesep = o.nodesep;
        return c;
    }

    created() {
        this.events = new EventEmitter();
    }

    mounted() {
        let cfg = localStorage['config.app'];
        if (cfg) Object.assign(this, JSON.parse(cfg));
        window.addEventListener('beforeunload', () =>
            localStorage['config.app'] = JSON.stringify(this.$data));
    }

    baseConfig() {
        switch (this.opts.mode) {
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
}
</script>