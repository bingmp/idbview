import{_ as f,b as n,o as m,q as w,w as e,e as r,m as a}from"./index-S2yEu14Y.js";const _={data(){return{activeName:"id1"}},methods:{handleClick(o,s){}}},d=a("iframe",{src:"./shinyserver/shinyrnaseq/01_dataselect_limma/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),h=a("iframe",{src:"./shinyserver/shinyrnaseq/02_limma/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),b=a("iframe",{src:"./shinyserver/enrichment/kegg/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),k=a("iframe",{src:"./shinyserver/enrichment/gseakegg/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),p=a("iframe",{src:"./shinyserver/enrichment/go/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),v=a("iframe",{src:"./shinyserver/enrichment/gseago/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),g=a("iframe",{src:"./shinyserver/enrichment/ora/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1);function y(o,s,z,C,t,u){const l=n("el-tab-pane"),c=n("el-tabs");return m(),w(c,{modelValue:t.activeName,"onUpdate:modelValue":s[0]||(s[0]=i=>t.activeName=i),type:"card",onTabClick:u.handleClick},{default:e(()=>[r(l,{label:"Data select",name:"id1"},{default:e(()=>[d]),_:1}),r(l,{label:"limma",name:"id2"},{default:e(()=>[h]),_:1}),r(l,{label:"KEGG",name:"id3"},{default:e(()=>[b]),_:1}),r(l,{label:"gsea-KEGG",name:"id4"},{default:e(()=>[k]),_:1}),r(l,{label:"GO",name:"id5"},{default:e(()=>[p]),_:1}),r(l,{label:"gsea-GO",name:"id6"},{default:e(()=>[v]),_:1}),r(l,{label:"ORA",name:"id7"},{default:e(()=>[g]),_:1})]),_:1},8,["modelValue","onTabClick"])}const N=f(_,[["render",y]]);export{N as default};
