import{d as w,k as y,r as c,o as r,g as q,w as s,h as n,a as o,c as m,e as v,u as p,F as k,S as E,f as D,m as S,q as x,t as V,O as C,P as N}from"./.pnpm-BcCp6Nip.js";import{u as B}from"./shiny-CbT69HWM.js";import{_ as I}from"./index-BKHEy8YK.js";const R=[{describe:"DEseq2: data DEG analysis",type:"RNAseq",img:"/src/assets/graphmed/rnaseq/deseq2.png",src:"https://db.chcmu.com.cn/idbview/rnaseq/DEseq2/"},{describe:"KEGG",type:"Enrichment",img:"/src/assets/graphmed/rnaseq/kegg.png",src:"https://db.chcmu.com.cn/idbview/enrichment/kegg/"},{describe:"gseakegg",type:"Enrichment",img:"/src/assets/graphmed/rnaseq/gseakegg.png",src:"https://db.chcmu.com.cn/idbview/enrichment/gseakegg/"},{describe:"go",type:"Enrichment",img:"/src/assets/graphmed/rnaseq/go.png",src:"https://db.chcmu.com.cn/idbview/enrichment/go/"},{describe:"gseago",type:"Enrichment",img:"/src/assets/graphmed/rnaseq/gseago.png",src:"https://db.chcmu.com.cn/idbview/enrichment/gseago/"},{describe:"ora",type:"Enrichment",img:"/src/assets/graphmed/rnaseq/ora.png",src:"https://db.chcmu.com.cn/idbview/enrichment/ora/"}],G=t=>(C("data-v-a4eb7212"),t=t(),N(),t),T=G(()=>o("iframe",{src:"https://db.chcmu.com.cn/idbview/rnaseq/01_dataselect_DEseq2/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1)),F={class:"volume-wrapper"},$=["onClick"],z=["src"],A=w({__name:"DEseq2",setup(t){let a=y("id1"),u=B(),g=D(),h=S();const _=async l=>{await u.Tools(l),g.push({path:"/shiny/tools",query:{redirect:h.path}})};return(l,i)=>{const d=c("el-tab-pane"),b=c("el-card"),f=c("el-tabs");return r(),q(f,{modelValue:p(a),"onUpdate:modelValue":i[0]||(i[0]=e=>E(a)?a.value=e:a=e),type:"card"},{default:s(()=>[n(d,{label:"Data select",name:"id1"},{default:s(()=>[T]),_:1}),n(d,{label:"Data analysis",name:"id2"},{default:s(()=>[o("div",F,[(r(!0),m(k,null,v(p(R),e=>(r(),m("div",{class:"volume-item",key:e.id,onClick:K=>_(e.src)},[n(b,{style:{"text-align":"center"}},{header:s(()=>[x(V(e.describe),1)]),default:s(()=>[o("img",{src:e.img,style:{width:"100%"}},null,8,z)]),_:2},1024)],8,$))),128))])]),_:1})]),_:1},8,["modelValue"])}}}),U=I(A,[["__scopeId","data-v-a4eb7212"]]);export{U as default};