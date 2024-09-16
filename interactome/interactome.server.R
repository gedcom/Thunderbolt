library(igraph)
library(networkD3)
library("writexl")
source("fileupload/fileupload.server.R", local = TRUE)

render.graph = function(name, gph)
{
  nodes.df = as.data.frame(get.vertex.attribute(gph))
  if (!"size" %in% colnames(nodes.df)) nodes.df = data.frame(nodes.df, size = 10)
  nodes.df$name = as.character(nodes.df$name)
  
  preyrows = nodes.df$group == "Prey"
  baitrows = nodes.df$group == "Bait"
  extrows = nodes.df$group == "External"
  
  nodes.df = rbind(nodes.df[preyrows,], nodes.df[baitrows,], nodes.df[extrows,])
  
  links.df = get.data.frame(gph)
  
  bait.ids = nodes.df$name[tolower(nodes.df$group) == "bait"]
  links.df$width = apply(links.df[,c("from", "to")], 1, function(row) {
    if (all(row %in% bait.ids)) return(0) else return(1)
  })
  links.df[,1] = match(links.df[,1], table = nodes.df$name) - 1
  links.df[,2] = match(links.df[,2], table = nodes.df$name) - 1
  
  # If links.df is empty, make a dummy row to keep forceNetwork from having a tantrum
  if (nrow(links.df) == 0)
    links.df = data.frame(from = 0, to = 0, width = 0)
  
  return(forceNetwork(Links = links.df, Nodes = nodes.df,
    Source = "from", Target = "to", Value = "width",
    NodeID = "name", Nodesize = "size", Group = "group",
    opacity = 1, charge = -180, legend = TRUE, zoom = TRUE,
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
    linkDistance = JS("function(d){
                if (d.value == 0) 
                {
                  return(200);
                } else {
                  return (50);
                }
             }")))
}

######################
######################
### SETUP FUNCTION ###
######################
######################
interactome.module = function(name, siglists) {
  # testoutput = paste0(name, "testoutput")
  
  mf = match.call()
  siglists = mf$siglists
  
  newsetbutton = paste0(name, "newsetbutton")
  newsetpopup = paste0(name, "newsetpopup")
  newsetback = paste0(name, "newsetback")
  newsetnext = paste0(name, "newsetnext")
  newsetname = paste0(name, "newsetname")
  uploadorsig = paste0(name, "uploadorsig")     # radiobutton - upload data or draw from significance list
  newsetspecies = paste0(name, "newsetspecies") # New set dialogue species input
  dispspec = paste0(name, "dispspec")           # Conditional variable for displaying species input
  newsetidtype = paste0(name, "newsetidtype")
  siglistseltbl = paste0(name, "siglistseltbl")
  fileuploadnm = paste0(name, "fu")
  sheetselect = paste0(name, "sheetselect")
  outputdisp = paste0(name, "disp")
  baitseltext = paste0(name, "baitseltext")
  baitseltable = paste0(name, "baitseltable")
  preyseltable = paste0(name, "preyseltable")
  previewgraph = paste0(name, "previewgraph")
  resetgraph = paste0(name, "resetgraph")   # New set dialogue, show graph preview of uploaded data
  
  # Main screen
  setlisttable = paste0(name, "setlisttable")  # List of protein sets/subgraphs
  delsetsbtn = paste0(name, "delsetsbtn")       # Delete selected sets
  clearsetsbtn = paste0(name, "clearsetsbtn")  # Clear all sets
  chkhprd = paste0(name, "chkhprd")             # Include links from HPRD database
  chkstring = paste0(name, "chkstring")         # Include links from string
  chklinkdbs = paste0(name, "chklinkdbs")       # Include links from HPRD/string database
  chklinktyp = paste0(name, "chklinktyp")       # Link types to show (internal/external)
  rungraphbtn = paste0(name, "rungraphbtn")     # Run graph
  
  graphview = paste0(name, "graphview")
  
  hubtable = paste0(name, "hubtable")
  bottlenecktbl = paste0(name, "bottlenecktbl")
  
  # Save hub and bottleneck data
  dwnlhubs = paste0(name, "dwnlhubs")
  dwnlbtw = paste0(name, "dwnlbtw")
  dnhubtype = paste0(name, "dnhubtype")
  dnbtwtype = paste0(name, "dnbtwtype")
  
  dwnlgph = paste0(name, "dwnlgph")
  dngphtype = paste0(name, "dngphtype")
  
  rct.list = list()
  rct.list$rv = reactiveValues(id.types = character(length = 0), popupdisp = "none",
                               graphinfo.name = character(0), graphinfo.origin = character(0), graphinfo.idtype = character(0),
                                                      gphlist = list(), graphinfo.species = character(0))
  rct.list$fu = fileupload.module(name = fileuploadnm)
  
  
  ##################
  # User interface #
  ##################
  
  # Protein list
  
  rct.list$startnewset = observeEvent(input[[newsetbutton]], {
    updateTextInput(session, newsetname, value = "")
    rct.list$rv$popupdisp = "source"
  })
  
  output[[dispspec]] <<- renderText ({
    if (length(rct.list$rv$graphinfo.name) > 0) "false" else "true"
  })
  outputOptions(output, dispspec, suspendWhenHidden = FALSE)
  
  output[[siglistseltbl]] <<- DT::renderDataTable({
    switch(input[[uploadorsig]],
      "sig" = {
        sigtable = eval(siglists)
        drop.cols = colnames(sigtable) %in% c("sig.rows", "excluded.rows", "descriptive")
        sigtable[!drop.cols]
      },
      "protlist" = {
        md = metadata$all.protlists
        data.frame("Name" = md$name, "Owner" = md$owner, stringsAsFactors = FALSE)
      }
    )
  }, options = list(lengthChange = FALSE, scrollX = T, scrollY = "300px", paging = FALSE, scrollCollapse = TRUE, dom = 't'),
    selection = list(target = "row", mode = "single"),
    rownames = FALSE)
  
  baitselproxy = DT::dataTableProxy(baitseltable)
  
  observeEvent(input[[paste0(baitseltable, "_columns_selected")]],
               updateTextInput(session, baitseltext, value = ""))

  observe(if (length(input[[baitseltext]]) > 0)
               DT::selectColumns(baitselproxy, NULL))
  
  rct.list$nextdisp = observeEvent(input[[newsetnext]], {
    switch(rct.list$rv$popupdisp,
      "source" = {
        nameexists = isTruthy(input[[newsetname]])
        sourceexists = isTruthy(rct.list$loadedtable())
        
        if (nameexists && sourceexists)
        {
          rct.list$rv$popupdisp = "bait"
        } else {
          actions = NULL
          if (!nameexists) actions = c(actions, "enter a name for this set of proteins")
          if (!sourceexists) actions = c(actions, "upload or select a set of proteins")
          
          message = paste0("Please ", paste(actions, collapse = " and "), ".")
          
          showModal(modalDialog(message, title = "Action required", easyClose = TRUE, size = "s", fade = FALSE), session = session)
        }
      },
      "bait" = {
        if (!(length(input[[paste0(baitseltable, "_columns_selected")]]) == 1 && input[[baitseltext]] != ""))
          rct.list$rv$popupdisp = "prey"
      },
      "prey" = {
        if (length(input[[paste0(preyseltable, "_columns_selected")]]) == 1)
          rct.list$rv$popupdisp = "preview"
      },
      "preview" = {
        rct.list$rv$graphinfo.name = c(rct.list$rv$graphinfo.name, input[[newsetname]])
        neworigin = switch(input[[uploadorsig]],
          "upload" = c(rct.list$rv$graphinfo.origin, paste("file:", rct.list$fu$fileinput()$name)),
          "sig" = c(rct.list$rv$graphinfo.origin, paste("list:", eval(siglists)$name[ input[[paste0(siglistseltbl, "_rows_selected")]] ])),
          "protlist" = c(rct.list$rv$setinfo.origin, paste("list:", metadata$all.protlists$name[ input[[paste0(siglistseltbl, "_rows_selected")]] ]))
        )
        rct.list$rv$graphinfo.origin = c(rct.list$rv$graphinfo.origin, neworigin)
        rct.list$rv$graphinfo.idtype = c(rct.list$rv$graphinfo.idtype, input[[newsetidtype]])
        rct.list$rv$graphinfo.species = input[[newsetspecies]]
        
        rct.list$rv$gphlist = c(rct.list$rv$gphlist, list(rct.list$preview.graph()))
        
        rct.list$rv$popupdisp = "none"
        toggleModal(session, newsetpopup, toggle = "close")
      }
    )
  })
  
  rct.list$lastdisp = observeEvent(input[[newsetback]], {
    switch(rct.list$rv$popupdisp,
      "source" = {
        rct.list$rv$popupdisp = "none"
          toggleModal(session, newsetpopup, toggle = "close")
      },
      "bait" = {
        rct.list$rv$popupdisp = "source"
      },
      "prey" = {
        rct.list$rv$popupdisp = "bait"
      },
      "preview" = {
        rct.list$rv$popupdisp = "prey"
      }
    )
  })
  
  output[[outputdisp]] <<- renderText(rct.list$rv$popupdisp)
  outputOptions(output, outputdisp, suspendWhenHidden = FALSE)
  
  rct.list$loadedtable = reactive ({
    
    switch(EXPR = input[[uploadorsig]],
      "upload" = {
        rct.list$fu$file.final()
      },
      "sig" = {
        sel.sig = input[[paste0(siglistseltbl, "_rows_selected")]]
        
        data.frame(eval(siglists)$descriptive[[sel.sig]], stringsAsFactors = FALSE)
      },
      "protlist" = {
        sel.protlist = input[[paste0(siglistseltbl, "_rows_selected")]]
        md = metadata$all.protlists[sel.protlist,]
        fl = open.file(md$filepath, "rdata")[[1]]
        cn = md$colnums[[1]]
        df = list()
        df$Gene = if (!is.na(cn$Gene[1])) fl[cn$Gene] else NULL
        df$Uniprot = if (!is.na(cn$Uniprot[1])) fl[cn$Uniprot] else NULL
        df$Entrez = if (!is.na(cn$Entrez[1])) fl[cn$Entrez] else NULL
        
        as.data.frame(df)
      }
    )
  })
  
  output[[baitseltable]] <<- DT::renderDataTable({
    rct.list$loadedtable()
  }, options = list(lengthChange = FALSE, scrollX = T, scrollY = "400px", scrollCollapse = TRUE, dom = 't'),
    selection = list(target = "column", mode = "single"),
    rownames = FALSE)
  
  output[[preyseltable]] <<- DT::renderDataTable({
    rct.list$loadedtable()
  }, options = list(lengthChange = FALSE, scrollX = T, scrollY = "400px", scrollCollapse = TRUE, dom = 't'),
    selection = list(target = "column", mode = "single"),
    rownames = FALSE)
  
  rct.list$preview.graph = reactive ({
    conv.db = switch(input[[newsetspecies]],
      "hsa" = {
        library(org.Hs.eg.db)
        org.Hs.eg.db        
      },
      "mmu" = {
        library(org.Mm.eg.db)
        org.Mm.eg.db
      }
    )
    
    db.name = switch(EXPR = input[[newsetspecies]],
      "hsa" = {
        "org.Hs.eg.db"
      },
      "mmu" = {
        "org.Mm.eg.db"
      }
    )
    
    baitselcols = paste0(baitseltable, "_columns_selected")
    preyselcols = paste0(preyseltable, "_columns_selected")
    
    bait.dat = if (input[[baitseltext]] != "")
    {
      rep(input[[baitseltext]], nrow(rct.list$loadedtable() ))
    } else if (!is.null(input[[baitselcols]]) && input[[baitselcols]] != input[[preyselcols]]) {
      bait.col = input[[baitselcols]] + 1
      as.character(rct.list$loadedtable()[,bait.col])
    } else {
      NULL
    }
    
    prey.col = input[[paste0(preyseltable, "_columns_selected")]] + 1
    prey.dat = as.character(rct.list$loadedtable()[,prey.col])
    
    nodes = union(prey.dat, bait.dat)
    symbols = if (input[[newsetidtype]] == "SYMBOL")
    {
      nodes
    } else {
      db.fn = paste(dbdir, "org.dbs", db.name, sep = "/")
      load(db.fn)
      idx = match(sig.ids$first, dat.conv[[ input[[newsetidtype]] ]], nomatch = NA)
      
      conv = dat.conv$SYMBOL[idx]
      conv[is.na(conv[,2]), 2] = paste0("(", conv[is.na(conv[,2]), 1], ")") # Use original ID for unconverted nodes
      conv[match(nodes, conv[,1]), 2]
    }
    
    
    
    prv.gph = make_empty_graph(directed = FALSE) + vertices(symbols)
    
    if (!is.null(bait.dat))
    {
      edge.mat = rbind(symbols[match(bait.dat, nodes)],
                       symbols[match(prey.dat, nodes)])
      
      prv.gph = prv.gph + edges(edge.mat)
    }
    
    vertex_attr(prv.gph, name = "idtype") = input[[newsetidtype]]
    vertex_attr(prv.gph, name = "orig.id") = nodes
    
    vertex_attr(prv.gph, name = "group") = "Prey"
    bait.nodes = which(nodes %in% bait.dat)
    
    if (length(bait.nodes) > 0)
    {
      vertex_attr(prv.gph, name = "group", index = unique(bait.dat)) = "Bait"
    }
    
#     vertex_attr(prv.gph, name = "size") = 10
#     edge_attr(prv.gph, name = "width") = 5
    prv.gph
  })

  output[[previewgraph]] <<- renderForceNetwork({
    render.graph("name", rct.list$preview.graph())
  })
  
  rct.list$graphinfotable = reactive ({
    data.frame("Name" = rct.list$rv$graphinfo.name, "Origin" = rct.list$rv$graphinfo.origin,
               "Species" = rct.list$rv$graphinfo.species)
  })
  
  output[[setlisttable]] <<- DT::renderDataTable (rct.list$graphinfotable(),
    options = list(lengthChange = FALSE, dom = 't'),
    selection = list(target = "row", mode = "multiple"), rownames = FALSE)
  
  rct.list$deleteSets = observeEvent (input[[delsetsbtn]], {
    del.rows = input[[paste0(setlisttable, "_rows_selected")]]
    
    rct.list$rv$graphinfo.name = rct.list$rv$graphinfo.name[-del.rows]
    rct.list$rv$graphinfo.origin = rct.list$rv$graphinfo.origin[-del.rows]
    rct.list$rv$graphinfo.idtype = rct.list$rv$graphinfo.idtype[-del.rows]
    rct.list$rv$gphlist = rct.list$rv$gphlist[-del.rows]
    
    if (length(rct.list$rv$graphinfo.name) == 0)
    {
      rct.list$rv$graphinfo.species = NULL
    }
  })
  
  rct.list$clearAllSets = observeEvent (input[[clearsetsbtn]], {
    rct.list$rv$graphinfo.name = NULL
    rct.list$rv$graphinfo.origin = NULL
    rct.list$rv$graphinfo.idtype = NULL
    rct.list$rv$graphinfo.species = NULL
    rct.list$rv$gphlist = NULL
  })
  

  #################################################
  # Combined graph of baits and significant preys #
  #################################################
  rct.list$basegraph = eventReactive (input[[rungraphbtn]], {
    
    args = rct.list$rv$gphlist
    validate(need(args, label = "subgraphs"))
    
    out = if (length(args) > 1)
    {   
#       args$byname = TRUE
      
      comb.gph = do.call(igraph::union, args)
      vertnames = vertex_attr(comb.gph, name = "name")
      
      verts = do.call(cbind, get.vertex.attribute(comb.gph))
      group.cols = grepl("group", colnames(verts), fixed = TRUE)
      
      group.mat = as.matrix(verts[,group.cols])
      group.mat[is.na(group.mat)] = "na"
      
      bait.verts = apply(group.mat, 1, function(v) {
        if (any(tolower(v) == "bait"))
          return (TRUE)
        else
          return (FALSE)
      })
      
      vertex_attr(comb.gph, name = "group") = "Prey"
      vertex_attr(comb.gph, name = "group", index = bait.verts) = "Bait"
      
      comb.gph
    } else args[[1]]
    
    out
  })
 
  #################################################
  # Prey-prey and prey-ext interactions from HPRD #
  #################################################
  rct.list$hprd.gphs = reactive ({
    
    # Progress message
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    
    progress$set(0, message = "Searching HPRD...")
    
    basegraph = rct.list$basegraph()
    
    sig.ids = vertex_attr(basegraph, name = "name")

    # Get HPRD data
    hprd.dat = read.table("BINARY_PROTEIN_PROTEIN_INTERACTIONS.txt", sep = "\t", stringsAsFactors = FALSE)
    hprd.sig.gids = cbind(hprd.dat[,1] %in% sig.ids, hprd.dat[,4] %in% sig.ids)
    sig.hprd.tf =  hprd.sig.gids[,1] & hprd.sig.gids[,2] # Interactions between significant proteins
    fdn.hprd.tf = xor(hprd.sig.gids[,1], hprd.sig.gids[,2])
    
    sig.hprd.idx = which(sig.hprd.tf)
    fdn.hprd.idx = which(fdn.hprd.tf)
    
    hprd.sig.links = hprd.dat[sig.hprd.idx, c(1,4)]
    hprd.fdn.links = hprd.dat[fdn.hprd.idx, c(1,4)]
    
    # Create graph of internal interactions
    gph.int = make_empty_graph(directed = FALSE) + vertices(sig.ids) + edges(t(hprd.sig.links))
    
    # Reorder matrix of first-degree-neighbour interactions so our significant proteins are all in the first (source) column
    fdn.sig.col = apply(hprd.sig.gids[fdn.hprd.idx,], 1, function(row) which(row))
    hprd.fdn.src = sapply(1:nrow(hprd.fdn.links), function(i) hprd.fdn.links[i, fdn.sig.col[i]])
    hprd.fdn.tgt = sapply(1:nrow(hprd.fdn.links), function(i) hprd.fdn.links[i, 3 - fdn.sig.col[i]])
    
    # Retain external proteins with links to 2 or more internal proteins
    ord.fdn.links = data.frame(src = hprd.fdn.src, target = hprd.fdn.tgt, stringsAsFactors = FALSE)
    keep.fdns = unique(ord.fdn.links$target[duplicated(ord.fdn.links$target)])
    ord.fdn.links = ord.fdn.links[ord.fdn.links$target %in% keep.fdns,]
    
    ext.nodes = unique(ord.fdn.links$target)
    gph.ext = make_empty_graph(directed = FALSE) + vertices(sig.ids) + vertices(ext.nodes) + edges(t(ord.fdn.links))
    
    vertex_attr(gph.ext, name = "group", index = ext.nodes) = "External"
    
    hprd.out = list(int = gph.int, ext = gph.ext)
    
    hprd.out
    
  })
  
  ###################################################
  # Prey-prey and prey-ext interactions from String #
  ###################################################
  rct.list$string.gphs = reactive ({
    orgabbr = rct.list$rv$graphinfo.species
    # load("interactome/string/hsa/9606__protein_links_confident.RData") # Load String interaction table (confident.links)
    stringfn = paste0("interactome/string/", orgabbr, "/", orgabbr, ".confident.links.RData")
    load(stringfn)
    # Progress message
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    
    progress$set(0, message = "Searching STRING...")
    
    # Get AnnotationDbi database (human/mouse)
    conv.db = switch(EXPR = rct.list$rv$graphinfo.species,
      "hsa" = {
        library(org.Hs.eg.db)
        org.Hs.eg.db
      },
      "mmu" = {
        library(org.Mm.eg.db)
        org.Mm.eg.db
      }
    )
    
    db.name = switch(EXPR = rct.list$rv$graphinfo.species,
      "hsa" = {
        "org.Hs.eg.db"
      },
      "mmu" = {
        "org.Mm.eg.db"
      }
    )
    
    basegraph = rct.list$basegraph()
    sig.ids = data.frame(symbols = get.vertex.attribute(basegraph)$name,
                         stringsAsFactors = FALSE)
    sig.ids$first = toupper(sapply(strsplit(x = sig.ids$symbols, split = ";", fixed = TRUE), function(x) x[1]))
    sig.ids = na.omit(sig.ids)
    
    db.fn = paste(dbdir, "org.dbs", db.name, sep = "/")
    load(db.fn)
    idx = match(sig.ids$first, dat.conv$SYMBOL, nomatch = NA)
    conversion = data.frame(SYMBOL = sig.ids$first, ENSEMBLPROT = dat.conv$ENSEMBLPROT[idx], stringsAsFactors = FALSE, row.names = NULL)
    conversion$orig = sig.ids$symbols
    
    sig.interactions = data.frame(src = confident.links$protein1 %in% conversion$ENSEMBLPROT, target = confident.links$protein2 %in% conversion$ENSEMBLPROT)
    sig.interactions$internal = sig.interactions$src & sig.interactions$target
    sig.interactions$external = (sig.interactions$src | sig.interactions$target) & !sig.interactions$internal
    
    int.interactors = as.matrix(confident.links[sig.interactions$internal, c("protein1", "protein2")])

    ext.interactors.unsorted = as.matrix(confident.links[sig.interactions$external, c("protein1", "protein2")])
    
    # Sort ext.interactors so significant proteins are in the source columns and external
    # proteins are in the target column.
    ext.interactors = cbind(t(ext.interactors.unsorted)[t(sig.interactions[sig.interactions$external,1:2])], t(ext.interactors.unsorted)[!t(sig.interactions[sig.interactions$external,1:2])])
    colnames(ext.interactors) = c("internal", "external")
    
    # Retain external proteins with multiple links only
    ext.common = unique(ext.interactors[duplicated(ext.interactors[,2]),2])
    ext.interactors = ext.interactors[ext.interactors[,2] %in% ext.common,]
    
    # Convert internal proteins to original name
    int.interactors[,1] = conversion$orig[match(int.interactors[,1], conversion$ENSEMBLPROT)]
    int.interactors[,2] = conversion$orig[match(int.interactors[,2], conversion$ENSEMBLPROT)]
    
    ext.interactors[,1] = conversion$orig[match(ext.interactors[,1], conversion$ENSEMBLPROT)]

    # Convert external proteins to original format using AnnotationDbi
    db.fn = paste(dbdir, "org.dbs", db.name, sep = "/")
    load(db.fn)
    idx = match(ext.interactors[,2], dat.conv$ENSEMBLPROT, nomatch = NA)
    
    ext.interactors[,2] = dat.conv$SYMBOL[idx]
    ext.interactors = na.omit(ext.interactors)
    
    ##############
    ### GRAPHS ###
    ##############
    
    # All significant proteins (Bait-prey interactions)
    gph.sig = basegraph
    
    # Prey-prey interactions
    gph.int = make_empty_graph(directed = FALSE) + vertices(unique(sig.ids$symbols)) + edges(t(int.interactors))

    # Known interactions between >= 2 significant preys and external proteins
    gph.ext = make_empty_graph(directed = FALSE) + vertices(unique(as.vector(ext.interactors))) + edges(t(ext.interactors))
    ext.nodes = ext.interactors[,2]
    vertex_attr(gph.ext, name = "group", index = ext.nodes) = "External"
    
    string.out = list(int = gph.int, ext = gph.ext)
    
    string.out
  })

  rct.list$final.graph = eventReactive (input[[rungraphbtn]], {

    include.graphs = list(rct.list$basegraph())
    
#     if ("hprd" %in% input[[chklinkdbs]])
#     {
#       include.graphs = c(include.graphs, rct.list$hprd.gphs()[ input[[chklinktyp]] ]) 
#     }
    # if ("string" %in% input[[chklinkdbs]])
    # {
      include.graphs = c(include.graphs, rct.list$string.gphs()[ input[[chklinktyp]] ]) 
    # }
    
    gph.fnl = do.call(igraph::union, include.graphs)
    
    # Determine final groups
    
    verts = do.call(cbind, get.vertex.attribute(gph.fnl))
    group.cols = grepl("group", colnames(verts), fixed = TRUE)

    group.mat = as.matrix(verts[,group.cols])
    group.mat[is.na(group.mat)] = "na"
    
    group.mat[is.na(group.mat)] = "na"
    
    fnl.grp = apply(group.mat, 1, function(row) {
      outgrp = if ("Bait" %in% row)
      {
        "Bait"
      } else if ("Prey" %in% row)
      {
        "Prey"
      } else if ("External" %in% row)
      {
        "External"
      }
      
      return(outgrp)
    })
    
    vertex_attr(gph.fnl, name = "group") = fnl.grp
    
    gph.fnl
  })


  rct.list$hub.tbl = reactive ({
    hubness = hub_score(rct.list$final.graph())$vector
    hubness = na.omit(hubness)
    hubs = sort(hubness, decreasing = TRUE)
    hub.table = data.frame(Node = names(hubs), Hubness = hubs, stringsAsFactors = FALSE)
    rownames(hub.table) = NULL
    
    hub.table
  })
  
  rct.list$btw.tbl = reactive ({
    btw = betweenness(rct.list$final.graph())
    btw = na.omit(btw)
    btlnx = sort(btw, decreasing = TRUE)
    btl.table = data.frame(Node = names(btw), Betweenness = btw, stringsAsFactors = FALSE)
    rownames(btl.table) = NULL
    
    btl.table
  })

  output[[graphview]] <<- renderForceNetwork ({
    gph = rct.list$final.graph()
    validate(need(gph, message = FALSE))
    input[[resetgraph]]
    render.graph("name", gph)
  })

  output[[hubtable]] <<- DT::renderDataTable ({
    rct.list$hub.tbl()
  }, options = list(lengthChange = FALSE, dom = 'ftp', order = list(1, 'desc'),
                    pageLength = 5), rownames = FALSE)

  output[[bottlenecktbl]] <<- DT::renderDataTable ({
    rct.list$btw.tbl()
  }, options = list(lengthChange = FALSE, dom = 'ftp', order = list(1, 'desc'),
                     pageLength = 5), rownames = FALSE)

  output[[dwnlhubs]] = downloadHandler (
    filename = function() {
      savetype = input[[dnhubtype]]
      fext = switch(savetype,
                    "xls" = ".xlsx",
                    "tsv" = ".txt",
                    "csv" = ".csv",
                    "rdata" = ".RData"
      )
      
      fn = paste0("hubs.", Sys.Date(), fext)
      return(fn)
    },
    content = function(fn) {
      savetype = input[[dnhubtype]]
      
      save.data = rct.list$hub.tbl()
      
      if (is.null(rownames(save.data)))
      {
        row.names = FALSE
        col.names = TRUE
      } else {
        row.names = TRUE
        col.names = NA
        xlcol.names = FALSE
      }
      
      switch(savetype,
            "xls" = write_xlsx(save.data, path = fn, col_names = xlcol.names),
            "tsv" = write.table(save.data, file = fn, sep = "\t", row.names = row.names, col.names = col.names),
            "csv" = write.table(save.data, file = fn, sep = ",", row.names = row.names, col.names = col.names),
            "rdata" = save(save.data, file = fn)
      )
    }
  )

  
  output[[dwnlgph]] = downloadHandler(
    filename = function() {
      fext = input[[dngphtype]]
      
      fn = paste0("network", Sys.Date(), ".", fext)
      return(fn)
    },
    content = function(fn) {
      savetype = input[[dngphtype]]
      
      save.data = rct.list$final.graph()
      
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      on.exit(progress$close())
      
      if (savetype %in% c("dot", "graphml"))
      {
        progress$set(0, message = paste0("Writing network to ", savetype, " format..."))
        igraph::write.graph(save.data, fn, format = savetype)
      } else  if (savetype %in% c("png", "pdf", "bmp")) {
        args = list(fn)
        names(args) = if (savetype == "pdf") "file" else "filename"
        
        progress$set(0, message = paste0("Plotting network to ", savetype, " file..."))
        do.call(savetype, args)
        plot(save.data)
        dev.off()
      } else if (savetype == "rdata") {
        my.graph = save.data
        save(my.graph, file = fn)
      }
    }
  )
  
  output[[dwnlbtw]] = downloadHandler (
    filename = function() {
      savetype = input[[dnbtwtype]]
      fext = switch(savetype,
                    "xls" = ".xlsx",
                    "tsv" = ".txt",
                    "csv" = ".csv",
                    "rdata" = ".RData"
      )
      
      fn = paste0("bottlenecks.", Sys.Date(), fext)
      return(fn)
    },
    content = function(fn) {
      savetype = input[[dnbtwtype]]
      
      save.data = rct.list$btw.tbl()
      
      if (is.null(rownames(save.data)))
      {
        row.names = FALSE
        col.names = TRUE
      } else {
        row.names = TRUE
        col.names = NA
        xlcol.names = FALSE
      }
      
      switch(savetype,
             "xls" = write.xlsx2(save.data, file = fn, col.names = xlcol.names),
             "tsv" = write.table(save.data, file = fn, sep = "\t", row.names = row.names, col.names = col.names),
             "csv" = write.table(save.data, file = fn, sep = ",", row.names = row.names, col.names = col.names),
             "rdata" = save(save.data, file = fn)
      )
    }
)

  return (rct.list)
}
