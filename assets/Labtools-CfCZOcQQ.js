import{_ as f,b as n,o as d,q as m,w as e,e as t,m as o}from"./index-ix3N5d8b.js";const _={data(){return{activeName:"id1"}},methods:{handleClick(s,l){}}},w=o("iframe",{src:"./idbview/lab/elisa/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),b=o("iframe",{src:"./idbview/lab/qpcr/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1),p=o("iframe",{src:"./idbview/lab/lung/",allowfullscreen:"true",webkitallowfullscreen:"true",mozallowfullscreen:"true",oallowfullscreen:"true",msallowfullscreen:"true"},null,-1);function k(s,l,v,C,r,u){const a=n("el-tab-pane"),c=n("el-tabs");return d(),m(c,{modelValue:r.activeName,"onUpdate:modelValue":l[0]||(l[0]=i=>r.activeName=i),type:"card",onTabClick:u.handleClick},{default:e(()=>[t(a,{label:"Elisa",name:"id1"},{default:e(()=>[w]),_:1}),t(a,{label:"qPCR",name:"id2"},{default:e(()=>[b]),_:1}),t(a,{label:"Lung function",name:"id3"},{default:e(()=>[p]),_:1})]),_:1},8,["modelValue","onTabClick"])}const N=f(_,[["render",k]]);export{N as default};