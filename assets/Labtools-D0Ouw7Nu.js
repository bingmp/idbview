import{u as l,a as m}from"./vue-router-BauBeqfh.js";import{u}from"./shiny-BVH5AcPI.js";import{d,ah as _,o,c as e,F as h,a7 as b,u as g,U as y,O as r,S as f,T as w,a as v}from"./@vue-JJjsWFC4.js";import{_ as x}from"./index-C0MaqNHA.js";import"./pinia-DxKmAzm6.js";import"./axios-Cm0UX6qg.js";import"./element-plus-gDGsSZFq.js";import"./lodash-es-CrQyXJRP.js";import"./@vueuse-BvQFniWC.js";import"./@element-plus-D5xpP_yz.js";import"./@popperjs-D9SI2xQl.js";import"./@ctrl-r5W6hzzQ.js";import"./dayjs-Cdlb6j_n.js";import"./async-validator-DKvM95Vc.js";import"./memoize-one-BdPwpGay.js";import"./normalize-wheel-es-B6fDCfyv.js";import"./@floating-ui-CtcGg-6p.js";import"./default-passive-events-BHG8ztru.js";import"./nprogress-DPi0kXQt.js";import"./vite-plugin-mock-DmzKHbMQ.js";import"./mockjs-DNXaZA0Z.js";import"./path-to-regexp-mOxPpBbg.js";const C=[{describe:"Elisa analysis",type:"labtools",img:"https://chcmu.com.cn/wp-content/uploads/2024/03/elisa.png",src:"./idbview/labtools/elisa/"},{describe:"Q-PCR analysis",type:"labtools",img:"https://chcmu.com.cn/wp-content/uploads/2024/03/qpcr.png",src:"./idbview/labtools/qpcr/"},{describe:"Lung function",type:"labtools",img:"https://chcmu.com.cn/wp-content/uploads/2024/03/lung.png",src:"./idbview/labtools/lung/"}],k={class:"volume-wrapper"},S=["onClick"],L=["src"],T=d({__name:"Labtools",setup(q){let a=u(),i=l(),c=m();const n=async s=>{await a.Tools(s),i.push({path:"/shiny/tools",query:{redirect:c.path}})};return(s,B)=>{const p=_("el-card");return o(),e("div",k,[(o(!0),e(h,null,b(g(C),t=>(o(),e("div",{class:"volume-item",key:t.id,onClick:N=>n(t.src)},[y(p,{style:{"text-align":"center"}},{header:r(()=>[f(w(t.describe),1)]),default:r(()=>[v("img",{src:t.img,style:{width:"100%"}},null,8,L)]),_:2},1024)],8,S))),128))])}}}),Z=x(T,[["__scopeId","data-v-18f46318"]]);export{Z as default};
