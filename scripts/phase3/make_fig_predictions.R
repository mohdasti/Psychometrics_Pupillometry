suppressPackageStartupMessages({library(ggplot2); library(patchwork); library(dplyr); library(tibble)})
pos <- c("Catch","1","2","3","4"); px <- c(0,1.6,2.6,3.6,4.6)
low <- c(-1.28,-1.05,-0.15,0.75,1.65)
high <- list(Criterion=c(-1.68,-1.45,-0.55,0.35,1.25),
             Sensitivity=c(-1.20,-1.05,-0.50,0.05,0.60),
             Capacity=c(-1.28,-1.55,-0.48,0.58,1.65))
d <- bind_rows(lapply(names(high), function(p) bind_rows(
  tibble(panel=p,position=pos,x=px,effort="Low",z=low),
  tibble(panel=p,position=pos,x=px,effort="High",z=high[[p]])))) |>
  mutate(P=pnorm(z), effort=factor(effort,levels=c("Low","High")))
chk <- d |> select(panel,position,effort,z,P) |> tidyr::pivot_wider(names_from=effort,values_from=c(z,P)) |>
  mutate(dz=z_High-z_Low, dP=P_High-P_Low)
print(chk |> mutate(across(c(P_Low,P_High,dP),~round(.x,3))), n=Inf)
dz <- function(p) chk$dz[chk$panel==p]
a <- c(
 TRUE,
 all(abs(dz("Criterion")+0.40)<1e-9),
 abs(dz("Sensitivity")[2])<1e-9 && all(diff(dz("Sensitivity")[2:5])<0),
 abs(dz("Capacity")[1])<1e-9 && abs(dz("Capacity")[5])<1e-9 && all(diff(dz("Capacity")[2:5])>0))
print(a); if(!all(a)) stop("assertion failed")
grid <- bind_rows(lapply(names(high), function(p){ g<-seq(1.6,4.6,by=0.02)
  bind_rows(tibble(panel=p,x=g,effort="Low",P=pnorm(approx(px[2:5],low[2:5],g)$y)),
            tibble(panel=p,x=g,effort="High",P=pnorm(approx(px[2:5],high[[p]][2:5],g)$y)))})) |>
  mutate(effort=factor(effort,levels=c("Low","High")))
arr <- chk |> mutate(x=px[match(position,pos)]) |> filter(abs(dP)>=0.02) |>
  mutate(y0=P_Low-sign(dP)*0.022, y1=P_High+sign(dP)*0.028) |> filter(abs(y1-y0)>=0.015)
cols <- c(Low="#5DADE2",High="#A23B72")
mk <- function(p, ylab, xlab){
  dd <- d |> filter(panel==p) |> mutate(xd=x+ifelse(effort=="Low",-0.06,0.06))
  g <- ggplot() +
    geom_vline(xintercept=0.8,colour="#BDBDBD",linewidth=0.3) +
    geom_line(data=filter(grid,panel==p),aes(x,P,colour=effort),linewidth=0.8) +
    geom_point(data=dd,aes(xd,P,colour=effort),shape=16,size=2.2) +
    scale_colour_manual(values=cols,name="Physical effort") +
    scale_x_continuous(breaks=px,labels=pos,limits=c(-0.4,5)) +
    scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".50",".75","1")) +
    labs(title=p,x=xlab,y=ylab) +
    theme_classic(base_size=9,base_family="sans") +
    theme(plot.title=element_text(face="bold",size=9.5,hjust=0.5),
          panel.grid.major.y=element_line(colour="#EEEEEE",linewidth=0.3),
          axis.text=element_text(size=8,colour="#3A3A3A"),axis.line=element_line(colour="#3A3A3A"),
          legend.position="bottom")
  if(p=="Criterion") g <- g + annotate("text",x=-0.3,y=0.99,label="Illustration; not data",hjust=0,size=2.6,fontface="italic",colour="#7F7F7F")
  g }
fig <- (mk("Criterion",'P("different")',NULL) | mk("Sensitivity",NULL,"Offset level (1 = smallest)") | mk("Capacity",NULL,NULL)) +
  plot_layout(guides="collect") + plot_annotation(tag_levels="A") &
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.spacing=unit(2,"pt"), plot.tag=element_text(face="bold",size=10))
ggsave("fig_predictions.png",fig,width=7.2,height=2.9,dpi=300,bg="white")
ggsave("fig_predictions.pdf",fig,width=7.2,height=2.9,bg="white")
