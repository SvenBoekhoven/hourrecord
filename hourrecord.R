library("ggplot2")
library("dplyr")
library("animation")
library("grid")

allhourdata <- read.csv("hour record.csv", sep=";", header=TRUE, dec=",")

hourrecord <- 51852
lapdistance <- 250

allhourdata <- allhourdata %>% mutate(Actual.laptimes.cummulative = cumsum(Actual.laptimes)) %>%
        mutate(Average.actual.laptimes = Actual.laptimes.cummulative/Id) %>%
        mutate(Target.laptimes=(60*60/(hourrecord/lapdistance))) %>%
        mutate(Target.laptimes.cummulative = cumsum(Target.laptimes)) %>%
        mutate(Difference = Actual.laptimes - Target.laptimes) %>%
        mutate(Difference.cummulative = cumsum(Difference)) %>%
        mutate(Projected.distance = (Id*lapdistance)/Actual.laptimes.cummulative*3.6) %>%
        mutate(Required.laptimes.for.record = 
                       ((60*60)-Actual.laptimes.cummulative)/((hourrecord-(lapdistance*Id))/lapdistance)) %>%
        mutate(Required.speed = 250/Required.laptimes.for.record*3.6) %>%
        mutate(laptime.to.slow = ifelse(Actual.laptimes < Required.laptimes.for.record, 0, Actual.laptimes))

begin_ani <- 150
end_ani <- 206

for (i in begin_ani:end_ani){
        var <- allhourdata[i,]
        
        hourdata <- allhourdata %>% filter(Id <= i)
        avg_color <- ifelse(var[,10]<hourrecord/1000, "#800000", "#006600")
        hline_avg <- data.frame(Id=1:206, avg=as.numeric(var[,5]))
        hline_req <- data.frame(Id=1:206, req=as.numeric(var[,11]))
        hline_rec <- data.frame(Id=1:206, rec=as.numeric(var[,6]))
        
        gg <- ggplot()
        
        gg <- gg + theme_bw() +
                theme(panel.background=element_rect(fill="#F0F0F0")) +
                theme(plot.background=element_rect(fill="#F0F0F0")) +
                theme(panel.border=element_rect(colour="#F0F0F0")) +
                theme(panel.grid.major=element_line(colour="#D0D0D0",size=.5)) 
        
        gg <- gg + ggtitle(paste("Hourrecord attempt Jack Bobridge: ", as.character(var[,2]))) +
                theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20)) +
                theme(legend.position="none") +
                ylab("Lap time") + xlab("Lap") +
                theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
                theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
                theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
                theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5))
        
        gg <- gg + theme(plot.margin = unit(c(1.5, 1.5, 1, 1), "cm")) +
                annotate("text",x=as.numeric(var[,1])+2,y=as.numeric(var[,3]),label=round(as.numeric(var[,3]),digits=3),colour="#000099", hjust = 0)
        
        ## Laptimes
        gg <- gg + geom_point(data=hourdata, aes(x=Id, y=Actual.laptimes), colour="#000099", size=3)

#         ## Average Laptimes
#         gg <- gg + geom_line(data=hourdata, aes(x=Id, y=Average.actual.laptimes), colour="#909090", size=1/2)
# 
        ## Required Laptimes
        gg <- gg + geom_line(data=hline_req, aes(x=Id, y=req), colour=avg_color, size=1, alpha=1/2) +
        annotate("text",x=206+2,y=as.numeric(var[,11]),label=round(var[,12],digits=3),colour=avg_color, hjust = 0, alpha=1/2) +
        annotate("text",x=0,y=as.numeric(var[,11]),label=round(var[,11],digits=3),colour=avg_color, hjust = 1, alpha=1/2) +
        annotate("text",x=206,y=as.numeric(var[,11])+0.075,label="Required for record",colour=avg_color, hjust = 1, alpha=1/2)
        
        ## Current Record line
        gg <- gg + geom_line(data=hline_rec, aes(x=Id, y=rec),colour="#909090", size=1, linetype=2) +
                annotate("text",x=206+2,y=as.numeric(var[,6]),label="51.852",colour="#909090", hjust = 0) +
        annotate("text",x=0,y=as.numeric(var[,6]),label=round(var[,6],digits=3),colour="#909090", hjust = 1)
        
        ## Average speed/laptime line
        gg <- gg + geom_line(data=hline_avg, aes(x=Id, y=avg),colour=avg_color, size=1, alpha=1/2) +
                annotate("text",x=206+2,y=as.numeric(var[,5]),label=round(var[,10],digits=3),colour=avg_color, hjust = 0, alpha=1/2) +
                annotate("text",x=0,y=as.numeric(var[,5]),label=round(var[,5],digits=3),colour=avg_color, hjust = 1, alpha=1/2) +
                annotate("text",x=1,y=as.numeric(var[,5])+0.075,label="Average",colour=avg_color, hjust = 0, alpha=1/2)
        
        ## Required laptimes text
#        gg <- gg + annotate("text",x=152,y=16.1,label=paste("Required laptime for record:", round(as.numeric(var[,11]), digits=3)),colour="#3C3C3C", hjust = 0)
        
        gg <- gg + scale_x_continuous(minor_breaks=0,breaks=seq(0,200,50),limits=c(-5,216)) +
                scale_y_continuous(minor_breaks=0,breaks=seq(16, 18.5, 0.5)) + #,limits=c(16, 18.5)) +
                coord_cartesian(ylim=c(15.9, 18.6)) + ## zoom only, no cutoff data
                theme(axis.ticks=element_blank()) 
        
        print(gg)
        
        ggsave(file=paste("pr",i,".jpg", sep=""))
}

files = sprintf('pr%d.png', begin_ani:end_ani)
# im.convert(files, output = 'hourrecord_jb.gif') 
