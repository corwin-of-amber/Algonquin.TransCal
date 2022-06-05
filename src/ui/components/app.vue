<template>
    <egraph :egraph="g"/>
</template>

<script>
import EGraph from './egraph.vue';


export default {
    data: () => ({g: undefined}),
    components: { 'egraph': EGraph }
}
</script>