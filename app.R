library(rgdal)
library(sp)
library(spdep)
library(ggplot2)
library(raster)
library(tmap)
library(plotly)
library(reshape2)
library(shiny)
library(rgeos)

#covid haftalık vaka sayısı ve turkiye shapefile verilerinin yüklenmesi
load("veri.Rdata")

#UI
#Arayüzün oluşturulması: Sayfa kenarında dosya yükleme(file input),
# Hafta girdi seçimi (SelectInput) ve il seçimi (SelectInput) girdilerinin drag and drop 
# şeklinde oluşturulması
# Uygulamanın anlatıldığı giriş sekmesi ve Output çıktılarının tabsetpanel olarak dizayn edilmesi
# İlk iki sekme ikili hafta karşışaltırılması olacağından column ile iki sütun olarak oluşturuldu.
#İkinci sekme için raporlama kısmı ise fluidRow ile ekstra bir satır olarak verildi.
#Son sekmede ise trendler illerin komşuları dahil olmak üzere oluşturuluyor.
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(width=4,fileInput("file1", "CSV dosyası yükleyin",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
                     selectInput("hafta", "Hafta seçiniz:", choices=names(vaka) ,selected=NULL,multiple=TRUE,selectize = TRUE),
            
            selectInput("il", "il seçiniz:", choices=vaka$NAME_1 ,selected="Eskişehir",multiple=FALSE,selectize = TRUE)),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Giriş",
                             h1("İl Vaka Sayıları Karşılaştırma Ve Raporlama Uygulaması"),
                             br(),
                             p("Bu uygulama Türkiye'deki il bazında yüz binde covid vaka 
                               sayılarının haftalık bazda karşılaştırılması ve raporlanması için dizayn edilmiştir. Tematik haritanın çizilebilmesi için sınıf limitleri 0,20,50,100 olarak alınmıştır.
                               Haftalık vaka sayıları Türkiye Cumhuriyeti Sağlık Bakanlığı sitesinden alınmıştır.",a("Covid verileri",href="https://covid19.saglik.gov.tr/"),
                               style="font-family: times,serif; font-size:16pt"),
                             p("Uygulama kullanıcının kendi verilerini CSV dosyası olarak yükleyebilme ve sistemde kayıtlı diğer hafta vaka sayıları
                               ile karşılaştırma imkanı sağlar.",strong("CSV dosyası yükleyin"), "bölümüne dosya yüklemesi başlık satırını da içeren virgülle ayrılmış dosya türünde yapılmalıdır.
                               Dosya yüklemesi yapılmadığında ise sadece sistemde bulunan 4 hafta arasında karşılaştırılma yapılabilir.",style="font-family: times,serif; font-size:16pt"),
                             p("1. ve 2. grafikler için ilgili sekmeler seçilerek",strong("Hafta seçiniz"),"bölümünden girdi seçimi yapılmalıdır.Girdiler backspace tuşu ile silinip, değiştirilebilir. İki hafta arasındaki farklar
                               karşılaştırılacağı için 1. ve 2. sekmelerdeki grafiklerin oluşturulmasında iki adet hafta seçilmelidir.",style="font-family: times,serif; font-size:16pt"),
                             p("Son sekmede ise seçilen il ve komşuları bazında trendler karşılaştırılacaktır. Bunun için hem",strong ("il seçiniz"), "hem",strong("Hafta seçiniz")," girdileri seçilmelidir.Bu sekmedeki grafik için ikiden fazla hafta seçilebilir.",style="font-family: times,serif; font-size:16pt")),
                    tabPanel("İki hafta il sınıflandırma karşılaştırması", fluidRow(
                            column(6, tmapOutput("map1", width = "100%", height = 800)),
                            column(6, tmapOutput("map2", width = "100%", height = 800)))),
                    tabPanel("Artış-azalış ve Artış-azalış yüzdeleri",fluidRow(
                             column(6,tmapOutput("map3", width = "100%", height = 800)),
                             column(6,tmapOutput("map4", width = "100%", height = 800))),
                             br(),
                             fluidRow(column(12,textOutput("otorapor")))),
                    tabPanel("İl ve komşuları haftalık vaka trendi", 
                             plotlyOutput("map5", width = "100%", height = 800))
                    
                  )
            )
    ))
#SERVER
server <- function(input, output,session) {
  
#Eğer kullanıcı kendi dosyasını yüklemiyor ise global env.daki data seti
# eğer yükleme yapıyor ise hem sistemdeki veri hem de yüklenen verinin birleşimi ile oluşturulan verinin
#reaktif olarak atanması
  mydata<-reactive({ InFile<-input$file1
  if(is.null(InFile)) {
    
    vaka[c(2:5)]
  }
  else { upl<-read.csv(input$file1$datapath, header=input$header,check.names = FALSE)
  temp<-cbind(vaka[c(2:5)],upl)
  temp
  }
  })
  
 #Dosya yüklemesi durumunda hafta girdisi seçiminin güncellenmesi için UpdateSelectizeInput kullanımı 
  #observe ya da observeEvent kullanılması gerekiyor ki update yapılabilsin.
  observe({
    updateSelectizeInput(session,"hafta", "Hafta seçiniz:",choices=names(mydata()),selected=c("27Şubat-5Mart","6Mart-12Mart"))
  })
  
  
 #NAME_1 ile yeniden reaktif değişken ataması: Daha sonra merge aşamasında NAME_1 gerekli 
  vakaf<-reactive({cbind(vaka["NAME_1"],mydata()[input$hafta])})
  
 # fark ve kategori reaktif değişkenlerinin oluşturulması
  frk<-reactive({
    (vakaf()[input$hafta[2]]-vakaf()[input$hafta[1]])*100/vakaf()[input$hafta[1]];
  })
  kat<-reactive({ifelse(frk()<0,"azalış","artış");
  })
#tüm değişkenlerin shapefile ile merge yapılmadan önce tek bir dataframede olarak birleştirilmesi
  vakare<-reactive({temp<-cbind(vakaf(),frk(),kat())
  
  colnames(temp)<-c("NAME_1",input$hafta,"frk","kat") 
  temp})
#neden koşul var: Çünkü iki hafat karşılaştırılması için iki hafta ya da fazla seçilebilir.
  turcovre<-reactive({validate(
    need(length(input$hafta)>=2, "İki adet hafta seçilmelidir (Trendler için tüm haftalar seçilebilir)"))
    merge(turkiye,vakare(),by="NAME_1")})
# rook tarzı il komşularının belirlenmesi  
  rook<-reactive({poly2nb(turcovre(),queen=FALSE)})
  secil<-reactive({tmp1<-turcovre()@data[c(which(turcovre()@data$NAME_1==input$il),rook()[[which(turcovre()@data$NAME_1==input$il)]]),]
  
  tmp1<-subset(tmp1,select=-c(kat,frk))
  tmp1
  })
  # ggplot tabanlı plotly grafiği için veriyi melt etmemiz gerekiyor.
  secgr<-reactive({melt(secil())})  

#ilk sekme ilk grafik çıktısı: koşul iki hafta seçilmedilir.    
    output$map1<-renderTmap({ 
      validate(
        need(length(input$hafta)==2, "İki adet hafta seçilmelidir"))
    # harita risk sınıflandırması için breaks kesim noktalarının belirlnemesi
      breaks<-c(0,20,50,100,Inf)
      # risk sınıflarının renkleri
    renk=c("blue","yellow","orange","red")
    tmap1<-tmap_mode("view")+
        tm_shape(turcovre()) +
        tm_fill(input$hafta[1],breaks = breaks,palette=renk,title=c(" İllere göre 100 binde vaka sayıları")) +
        tm_borders() +
        tm_text("NAME_1")+
      tm_add_legend("fill",col=renk,title=c(" İllere göre risk sınıflandırılması"),
                    labels = c('Düşük risk','Orta Risk','Yüksek Risk','Çok Yüksek Risk'))           
    })
  
    
    #ilk sekme ikinci grafik çıktısı: koşul iki hafta seçilmedilir.    
    output$map2<-renderTmap({ 
      validate(
        need(length(input$hafta)==2, "İki adet hafta seçilmelidir"))
      breaks<-c(0,20,50,100,Inf)
      renk=c("blue","yellow","orange","red")
      tmap1<-tmap_mode("view") + 
        tm_shape(turcovre()) +
        tm_fill(input$hafta[2],breaks = breaks,palette=renk,title=c(" İllere göre 100 binde vaka sayıları")) +
        tm_borders() +
        tm_text("NAME_1")+
        tm_add_legend("fill",col=renk,title=c(" İllere göre risk sınıflandırılması"),
                      labels = c('Düşük risk','Orta Risk','Yüksek Risk','Çok Yüksek Risk'))  
    })
    
  # 2. sekme ilk grafik vaka değişim durumu kategorik artış azalış durumunun belirlenmesi
    output$map3<-renderTmap({ req(turcovre());
      tmap_mode("view") +
        tm_shape(turcovre()) +
        # midpoint 0 kesim noktası pozitif ya da negatif olması gerekiyor
        tm_fill("kat",
                title=c("Vaka değişim durum"),midpoint=0,style="cat",pal="Dark2") +
        tm_borders() +
        tm_text("NAME_1")
    })
    #2. sekme 2. grafik çıktısı
    output$map4<-renderTmap({ req(turcovre());
      tmap_mode("view")+
        tm_shape(turcovre()) +
        tm_fill("frk",
                title=c("Vaka yüzde değişim"),midpoint=0,pal="RdYlGn") +
        tm_borders() +
        tm_text("NAME_1")
    
    })
    # rapor çıktısı
    output$otorapor<-renderPrint({req(turcovre());
      rapor<-data.frame()
      for(i in 1:dim(turcovre()@data)[1])
      {rapor<-c(rapor,paste(turcovre()@data$NAME_1[i],"ilinde karşılaştırılan haftalara göre yüzde" ,abs(turcovre()@data$frk[i]), turcovre()@data$kat[i],"olmuştur"));
      }
    rapor})
      
    #3. sekme il trend grafiği
    output$map5<-renderPlotly({
      
      p <- ggplot(secgr(), aes(x=variable, y=value, colour=NAME_1, group=NAME_1 )) +
        xlab("haftalar")+ ylab("İllere göre 100000de vaka sayıları")+labs(colour = "İLLER")+
        geom_line() + geom_point()
      
      p<-p+theme(axis.title.x = element_text(face="bold", color="#993333", 
                                                  size=14),
                 axis.title.y = element_text(face="bold", color="#993333", 
                                                  size=14))
      
          fig <- ggplotly(p)
      
         fig 
      
    })
      
}
#Eğer mozilla ile çalıştırılmak istenirse R studiodan 
#Runapp yerine aşağıdaki iki kod yazılıp ShinyApp iptal edilerek konsoldan çalıştırılmalıdır.
#options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#runApp(list(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)
