<template>
    <egraph :egraph="g"/>
</template>

<script>
import Egraph from './egraph.vue';


export default {
    data: () => ({g: undefined}),
    components: { Egraph }
}
</script>

<style>

</style>