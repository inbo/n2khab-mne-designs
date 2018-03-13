#########################
## kleurschalen
#########################

kleurschaal <- c(
    HM = colors()[172],
    H = colors()[96],
    M = colors()[33],
    L = colors()[90],
    S = colors()[102],
    P = colors()[105]
)

kleurlabels <- c(
    HM = "HM: hoge of matige invloed",
    H = "H: hoge invloed",
    M = "M: matige invloed",
    L = "L: lage (nog betekenisvolle) invloed",
    S = "S: geen betekenisvolle invloed,\nmaar type is gevoelig",
    P = "P: geen betekenisvolle invloed,\nmaar type is potentieel gevoelig"
)

olympic_colours <- c("a" = "#FFE925", "b" = "goldenrod1", "c" = "seashell3", "d" = "#C7AB90")
olympic_labels <- c("a" = "Hoogst (a) --> wordt\neen hoofdmeetnet", "b" = "Minder hoog (b)", "c" = "Lager (c)", "d" = "Laagst (d)")
whh2levels <- c("Droog tot vochtig",
                "Droog tot vochtig + Tijdelijk tot permanent nat",
                "Tijdelijk tot permanent nat",
                "Oppervlaktewater")
whh2schaal <- c(0.1, 0.3, 0.5, 1) # alpha waarden
whh2schaal2 <- c("grey90", "grey70", "grey50", "black") # kleurwaarden
names(whh2schaal) <- whh2levels
names(whh2schaal2) <- whh2levels
whh2labels <- whh2levels
names(whh2labels) <- whh2levels
whh2labels[2] <- "Droog tot vochtig +\nTijdelijk tot permanent nat"

#########################
## HMLSP-tileplot
#########################


myraster <- function(mydata, mytitle){

    aantaldruk <- length(unique(mydata$Druk))
    aantalvegcode <- length(unique(mydata$Vegcode))

    typeklasseschaal <- RColorBrewer::brewer.pal(10, "Paired")
    names(typeklasseschaal) <- levels(milieudruk_rangschikking$Typeklasse)

    drukgroepering <-
        tibble(
            Druk = unique(mydata$Druk)
        ) %>%
        mutate(
            Druk = factor(Druk,
                          levels = (-scores(kruisdruk_CA)$species[,"CA1"] %>%
                                        sort %>%
                                        names))
        )

    vegcodegroepering <-
        tibble(
            Vegcode = unique(mydata$Vegcode)
        ) %>%
        inner_join(
            distinct(milieudruk_rangschikking, Vegcode, Typeklasse)
        ) %>%
        mutate(
            Vegcode = factor(Vegcode,
                             levels = (scores(kruisdruk_CA)$sites[,"CA1"] %>%
                                           sort %>%
                                           names))
        )



    ggplot() +
        geom_tile(data = mydata, aes(x = Druk, y = Vegcode, fill = Categorie)) +
        scale_fill_manual(values = kleurschaal,
                          labels = kleurlabels,
                          guide = guide_legend(title = "Invloedrelatie")) +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0),
            panel.background = element_blank(),
            legend.key = element_rect(fill = "white", colour = "white")
        ) +

        geom_tile(data = expand.grid(X = 1:aantaldruk, Y = 1:aantalvegcode),
                  aes(x = X, y = Y),
                  colour = "grey60",alpha = 0) +

        annotate(    # truukje om ruimte aan zijkant te maken
            geom = "point",
            x = -2, y = aantalvegcode, alpha = 0
        ) +

        # geom_point(data = drukgroepering,
        #     aes(x = Druk, y = aantalvegcode + 2 , alpha = Milieudrukkengroep),
        #     size = 3, colour = colors()[30]
        # ) +
        # scale_alpha_manual(values = c("1"=1, "2"=0.5, "3"=0.25, "4"=0.1)) +

        geom_point(data = vegcodegroepering,
                   aes(x = -1, y = Vegcode, colour = Typeklasse),
                   size = 2
        ) +
        scale_colour_manual(values = typeklasseschaal) +

        scale_x_discrete(position = "top") +

        labs(title = mytitle, x = "Milieudruk", y = "Type")


}



#########################
## HMLSP-tileplot met WWH2
#########################


myraster_whh2 <- function(mydata, mytitle){

    aantaldruk <- length(unique(mydata$Druk))
    aantalvegcode <- length(unique(mydata$Vegcode))

    typeklasseschaal <- RColorBrewer::brewer.pal(10, "Paired")
    names(typeklasseschaal) <- levels(milieudruk_rangschikking$Typeklasse)

    veglevels_doelpop <-
        mydata %>%
        distinct(Waterhuishoudingsklasse_2, Typeklasse, Vegcode) %>%
        mutate(Vegcode = as.character(Vegcode)) %>%
        arrange(desc(Waterhuishoudingsklasse_2), desc(Typeklasse), desc(Vegcode)) %>%
        .$Vegcode

    vegcodegroepering <-
        mydata %>%
        distinct(Waterhuishoudingsklasse_2, Typeklasse, Vegcode) %>%
        mutate(Vegcode = as.character(Vegcode)) %>%
        arrange(desc(Waterhuishoudingsklasse_2), desc(Typeklasse), desc(Vegcode)) %>%
        mutate(
            Vegcode = factor(Vegcode,
                             levels = veglevels_doelpop)
        )

    ggplot() +
        geom_tile(data = mydata, aes(x = Druk, y = Vegcode, fill = Categorie)) +
        scale_fill_manual(values = kleurschaal,
                          labels = kleurlabels,
                          guide = guide_legend(title = "Invloedrelatie")) +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0),
            panel.background = element_blank(),
            legend.key = element_rect(fill = "white", colour = "white")
        ) +

        geom_tile(data = expand.grid(X = 1:aantaldruk, Y = 1:aantalvegcode),
                  aes(x = X, y = Y),
                  colour = "grey60",alpha = 0) +

        annotate(    # truukje om ruimte aan zijkant te maken
            geom = "point",
            x = -2, y = aantalvegcode, alpha = 0
        ) +

        geom_point(data = vegcodegroepering,
                   aes(x = -1.5, y = Vegcode, alpha = Waterhuishoudingsklasse_2),
                   size = 2,
                   colour = "black"
        ) +

        geom_point(data = vegcodegroepering,
                   aes(x = -0.5, y = Vegcode, colour = Typeklasse),
                   size = 2
        ) +

        scale_colour_manual(values = typeklasseschaal) +

        scale_alpha_manual(values = whh2schaal,
                           labels = whh2labels) +

        scale_x_discrete(position = "top") +

        labs(title = mytitle,
             x = "Milieudruk",
             y = "Type")


}





#########################
## grafiek met aantal standplaatsfactoren
#########################

whh_facetlabels <- c("Droog tot vochtig" = "Droog tot vochtig",
                     "Tijdelijk tot permanent nat" = "Tijdelijk tot\npermanent nat",
                     "Oppervlaktewater" = "Oppervlaktewater")


compplot2 <- function(df) {
    df %>%
        ggplot(aes(x = Compartiment, y = Druk)) +
        geom_tile(aes(fill = Milieudrukkengroep), colour = "grey30") +
        geom_text(aes(label = AantalStplf, alpha = Pnabijheid),
                  colour = "purple4", size = 4, fontface = "bold") +
        # geom_point(aes(alpha = Pnabijheid),
        #            size = 2) +
        scale_alpha_manual(values = c(1, 0.3), breaks = c(
            "Compartiment\nmet maximale\nP-nabijheid",
            "Compartiment\nmet lagere\nP-nabijheid")) +
        scale_fill_manual(values = olympic_colours) +
        scale_x_discrete(position = "top") +
        theme(
            axis.text.x = element_text(angle = 90, hjust = 0)
        )     +
        facet_wrap(~Waterhuishouding,
                   nrow = 1,
                   scales = "free_x",
                   labeller = as_labeller(whh_facetlabels)) +
        labs(x = "Milieucompartiment",
             y = "Milieudruk")
}

#########################
## grafiek met standplaatsfactoren per milieudruk
#########################

stpl_plot_whh <- function(df) {
    df %>%
        ggplot(aes(x = Standplaatsfactor, y = Druk)) +
        geom_tile(aes(fill = MaxMilieudrukkengroep), colour = "grey30") +
        geom_text(aes(label = Waterhuishouding_afk,
                      colour = Waterhuishouding_afk,
                      alpha = Pnabijheid),
                  size = 3,
                  fontface = "bold",
                  position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = olympic_colours,
                          labels = olympic_labels,
                          guide = guide_legend(title = "Rang milieudruk\nin beschouwd\nmilieucompartiment")) +
        scale_colour_manual(values = c("firebrick4", "darkgreen", "blue"),
                            breaks = c("D", "N", "O"),
                            labels = c("D" = "D: droog tot vochtig",
                                       "N" = "N: tijdelijk tot\npermanent nat",
                                       "O" = "O: oppervlaktewater"),
                            guide = guide_legend(title = "Waterhuishoudingsklasse")) +
        scale_alpha_manual(values = c(1, 0.3), breaks = c(
            "Compartiment\nmet maximale\nP-nabijheid",
            "Compartiment\nmet lagere\nP-nabijheid")) +
        scale_x_discrete(position = "top") +
        theme(
            axis.text.x = element_text(angle = 80, hjust = 0)
        ) +
        labs(y = "Milieudruk")
}



#########################
## grafiek met standplaatsfactoren per type
#########################



stpl_plot_types <- function(df) {
    vegcodegroepering <-
        df %>%
        distinct(Waterhuishoudingsklasse_2, Typeklasse, Vegcode) %>%
        mutate(Vegcode = as.character(Vegcode)) %>%
        arrange(desc(Waterhuishoudingsklasse_2), desc(Typeklasse), desc(Vegcode)) %>%
        mutate(
            Vegcode = factor(Vegcode,
                             levels = .$Vegcode)
        )

    aantalvegcode <- vegcodegroepering %>% nrow
    aantalstplf <- df %>% distinct(Standplaatsfactor_Comp) %>% nrow

    typeklasseschaal <- RColorBrewer::brewer.pal(10, "Paired")
    names(typeklasseschaal) <- levels(milieudruk_rangschikking$Typeklasse)

    df %>%
        ggplot(aes(x = Standplaatsfactor_Comp, y = Vegcode)) +
        geom_tile(aes(fill = MaxMaxMilieudrukkengroep), colour = "grey30") +
        geom_text(aes(label = AantalDrukken), size = 2.5, colour = "purple2") +

        scale_fill_manual(values = olympic_colours,
                          labels = olympic_labels,
                          guide = guide_legend(title = "Hoogst voorkomende rang van\nmet standplaatsfactor\nbeoogde milieudrukken\n(paars getal: aantal\nbeoogde milieudrukken)")) +

        geom_tile(data = expand.grid(X = 1:aantalstplf, Y = 1:aantalvegcode),
                  aes(x = X, y = Y),
                  colour = "grey60",alpha = 0) +

        annotate(    # truukje om ruimte aan zijkant te maken
            geom = "point",
            x = -2, y = aantalvegcode, alpha = 0
        ) +

        geom_point(data = vegcodegroepering,
                   aes(x = -1.5, y = Vegcode, alpha = Waterhuishoudingsklasse_2),
                   size = 2,
                   colour = "black"
        ) +

        geom_point(data = vegcodegroepering,
                   aes(x = -0.5, y = Vegcode, colour = Typeklasse),
                   size = 2
        ) +

        scale_colour_manual(values = typeklasseschaal) +

        scale_alpha_manual(values = whh2schaal,
                           labels = whh2labels) +

        scale_x_discrete(position = "top") +

        theme(
            axis.text.x = element_text(angle = 60, hjust = 0),
            panel.background = element_blank()
        ) +
        labs(x = "Standplaatsfactor per milieucompartiment", y = "Type")
}



