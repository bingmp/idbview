import{i as s}from"./index-CrFcgUJR.js";import{_ as r,o as i,c as n}from"./index-ix3N5d8b.js";const c={name:"Chart",mounted(){this.initChart()},methods:{initChart(){const e=s(document.getElementById("achart2")),a={color:["#5470C6","#EE6666","purple","#5470C6","#EE6666"],tooltip:{trigger:"axis",axisPointer:{type:"cross",crossStyle:{color:"#999"}}},toolbox:{feature:{dataView:{show:!0,readOnly:!1},magicType:{show:!0,type:["line","bar"]},restore:{show:!0},saveAsImage:{show:!0,pixelRatio:15}}},legend:{top:"top",data:["HAdV-3 cases","HAdV-7 cases","HAdV positivte cases","Percentage of positive cases","HAdV-3","HAdV-7"]},xAxis:[{type:"category",data:["2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],axisPointer:{type:"shadow"}}],yAxis:[{type:"value",name:"Number of suspected cases",min:0,max:150,interval:10,axisLabel:{formatter:"{value}"}},{type:"value",name:"Percentage of positive cases",min:0,max:90,interval:10,position:"right",axisLabel:{formatter:"{value} %"}}],series:[{name:"HAdV-3 cases",type:"bar",stack:"Total",tooltip:{valueFormatter:function(t){return t}},data:[10,31,16,12,11,32,4,9,23,5,24,0,6,14]},{name:"HAdV-7 cases",type:"bar",stack:"Total",tooltip:{valueFormatter:function(t){return t}},data:[0,25,41,18,14,16,3,1,1,4,14,12,0,0]},{name:"HAdV positivte cases",type:"bar",stack:"Total",tooltip:{valueFormatter:function(t){return t}},data:[7,6,36,19,34,5,3,11,3,8,18,43,15,18]},{name:"HAdV-3",type:"line",yAxisIndex:1,offset:80,tooltip:{valueFormatter:function(t){return t+" %"}},data:[58.82,50,17.2,24.49,18.64,60.38,40,42.86,85.19,29.41,42.86,0,28.57,43.75]},{name:"HAdV-7",type:"line",yAxisIndex:1,tooltip:{valueFormatter:function(t){return t+" %"}},data:[0,40.32,44.09,36.73,23.73,30.19,30,4.76,3.7,23.53,25,21.82,0,0]}]};e.setOption(a)}}},l={id:"achart2",style:{height:"600px"}};function p(e,o,a,t,d,u){return i(),n("div",l)}const x=r(c,[["render",p]]);export{x as default};
