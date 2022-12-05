/**
 * Some facilities for easier development & introspection when using
 * Vue components.
 */
import * as Vue from 'vue';


/**
 * Allows to use a Vue component as a root component, giving access
 * to its properties (normally read-only) by wrapping it with a container
 * component that stores the component state as data (which is mutable
 * and reactive).
 * @param component a Vue component definition
 * @returns a wrapper component definition (can be passed to `Vue.createApp`)
 */
 function componentAdapter(component: {props: string[]}) {

    let propNames = component.props;  /** @todo handle props given as an object */

    return {
        data: () => Object.fromEntries(propNames.map(k => [k, undefined])),
        render(_ctx, _cache) {
            let props = Object.fromEntries(propNames.map(k => [k, _ctx[k]]));
            return (/*Vue.openBlock(), */
                Vue.createBlock(component, {ref: 'it', ...props}, null, 
                    8 /* patchFlag = PROPS */, propNames))
        }
    };
}

interface Component<P, HostElement = Element> extends Vue.App {
    mount(rootContainer: HostElement | string, isHydrate?: boolean, isSVG?: boolean): Vue.ComponentPublicInstance<P>;
}

function createComponent<P = {}>(component: {props: string[]}, rootProps?: any) {
    return Vue.createApp(componentAdapter(component), rootProps) as Component<P>;
}

function withProps<T>() {
    return <U>(u: U) => u as U & T;
}


export { componentAdapter, createComponent, withProps }