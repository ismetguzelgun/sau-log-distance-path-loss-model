require("RGtk2")
library ( cairoDevice )
pencere<-gtkWindow()
pencere["title"]<-"Log-Distance Path Loss"

cerceve<-gtkFrameNew("Log-Distance Path Loss Hesapla")
pencere$add(cerceve)

#box ekler
box1<-gtkVBoxNew()
box1$setBorderWidth(30)
cerceve$add(box1)

box2<-gtkHBoxNew(spacing=10)#distance between elements
box2$setBorderWidth(24)

#disaridan alinan degerler icin

label3=gtkLabelNewWithMnemonic("*PTdegeri (mW)") #text label
box1$packStart(label3)

PTdegeri<-gtkEntryNew()#text field with expression
PTdegeri$setWidthChars(25)
box1$packStart(PTdegeri)
##dBm cinsinden PT
label6=gtkLabelNewWithMnemonic("PT degeri(dBm)") #text label
box1$packStart(label6)

PTDbmdegeri<-gtkEntryNew()#text field with expression
PTDbmdegeri$setWidthChars(25)
box1$packStart(PTDbmdegeri)
##d referans mesafe
label7=gtkLabelNewWithMnemonic("*D Referans degeri(metre)") #text label
box1$packStart(label7)

D0Rdegeri<-gtkEntryNew()#text field with expression
D0Rdegeri$setWidthChars(25)
box1$packStart(D0Rdegeri)
##d mesafe
label4=gtkLabelNewWithMnemonic("*D degeri (metre)") #text label
box1$packStart(label4)

D0degeri<-gtkEntryNew()#text field with expression
D0degeri$setWidthChars(25)
box1$packStart(D0degeri)

#PL0Degeri
label2=gtkLabelNewWithMnemonic("PL0 Referans Degeri") #text label
box1$packStart(label2)

PL0Degeri<-gtkEntryNew()#text field with result of our calc
PL0Degeri$setWidthChars(25)
box1$packStart(PL0Degeri)

#PLHDegeri
label5=gtkLabelNewWithMnemonic("PLH Degeri") #text label
box1$packStart(label5)

PLHDegeri<-gtkEntryNew()#text field with result of our calc
PLHDegeri$setWidthChars(25)
box1$packStart(PLHDegeri)

#sonuc PTdegeri-PLdegeri
label=gtkLabelNewWithMnemonic("Sonuc") #text label
box1$packStart(label)

sonuc<-gtkEntryNew()#text field with result of our calc
sonuc$setWidthChars(25)
box1$packStart(sonuc)

#box2
box2<-gtkHBoxNew(spacing=10)#distance between elements
box2$setBorderWidth(24)
box1$packStart(box2)

Hesapla<-gtkButton("Hesapla")
box2$packStart(Hesapla,fill=F) #button which will start calculating

#GrafCiz<-gtkButton("GrafCiz")
#box2$packStart(GrafCiz,fill=F) #button which will start calculating

NDegeri<-rGtkDataFrame(c("Serbest Uzay:				2","Kentsel Alan:				2.7","Golgeli Kentsel Alan:		3","Bina ici direk gorus var:	1.6","Bina ici direk gorus yok:	4","Fabrika ici direk gorus yok:	3.1"))
combobox<-gtkComboBox(NDegeri)#combobox allowing to decide whether we want result as integer or duble

crt<-gtkCellRendererText()
combobox$packStart(crt)
combobox$addAttribute(crt,"text",0)

gtkComboBoxSetActive(combobox,0)
box2$packStart(combobox)

#######################################

DoHesapla<-function(button)
{

	DoMin<-function(deger)
	{
		#lambda değerini sabit 1/3 alarak yaptım hesaplamayı
		PL1Degeri<-20*log10(4*pi*(as.numeric(D0Rdegeri$getText()))*3)
		PL0Degeri$setText(PL1Degeri)
		#cc<-10*dd*log(5000/1000)
		#x<-c(PL1Degeri)
		#for(i in 2:5)
		#{
		#	deneme<-20*log10(4*pi*(as.numeric(D0Rdegeri$getText())*i)*3)
		#	x<-append(x,deneme)
		#}
					#sonuc$setText(as.integer(eval(parse(text=hesaplanacakDeger$getText()))))
				#grafCiz(PL1Degeri)	
					#group<-data.frame(c(D0degeri$getText()))
					
		PLDegeri<-PL1Degeri +10*deger*log10(as.numeric(D0degeri$getText())/as.numeric(D0Rdegeri$getText()))
		y<-c(PLDegeri)
		for(i in 2:5)
		{
			deneme<-PL1Degeri +10*deger*log10((as.numeric(D0degeri$getText())*i)/(as.numeric(D0Rdegeri$getText())))
			y<-append(y,deneme)
		}
		grafCiz(y,deger)
		PLHDegeri$setText(PLDegeri)
					#PT GOES
		PTTdegeri<-10*log10(as.numeric(PTdegeri$getText()))
		ssonuc<-PTTdegeri- PLDegeri
		PTDbmdegeri$setText(PTTdegeri)
					
		sonuc$setText(ssonuc)
	
	}

   #display error if R fails at calculating
   
			#PTdegeri<-PTdegeri$setText(as.integer(eval(parse(text=PTdegeri$getText()))))
			#D0degeri$setText((eval(parse(text=D0degeri$getText()))))
			#as.numeric(PTdegeri$getText())*10^3
	if(gtkComboBoxGetActive(combobox)==0)
	{		
		DoMin(2)
		
	}
	else if(gtkComboBoxGetActive(combobox)==1)
	{
		DoMin(2.7)
		
	}
	else if(gtkComboBoxGetActive(combobox)==2)
	{
		DoMin(3)
	}
	else if(gtkComboBoxGetActive(combobox)==3)
	{
		DoMin(1.6)
	}
	else if(gtkComboBoxGetActive(combobox)==4)
	{
		DoMin(4)
	
	}
	else
	{
		DoMin(3.1)
	}

}
grafCiz<-function(PLH,dd)
{
	da<-gtkDrawingArea()
	asCairoDevice(da)
	da$setSizeRequest ( 600 , 400)
	ww<-gtkWindow(show=FALSE)
	ww$add(da)
	ww$showAll()
	x<-rnorm(10)
	plot(PLH,main=dd,xlab="Mesafe Ornekleri(5)x1000 (m)",type="l")
	#plot(1:10,but1,type="l")
	#plot(1:but1,type="l")
}
gSignalConnect(Hesapla, "clicked", DoHesapla)
#gSignalConnect(GrafCiz, "clicked", grafCiz)