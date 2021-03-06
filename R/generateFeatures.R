#' generate_features function
#'
#' This function generates features from a given snapshot file
#' @param path where to look for the MOCCA extended snapshot 
#' @param seed (currently unused) rng seed
#' @param diagno.on TRUE to run additional diagnostics
#' @param cluster.plot TRUE to plot the cluster, completeness, FOV, etc
#' @param incomplete.gray color for plotting stars discarded due to completeness
#' @param complete.black color for plotting stars not discarded
#' @param FOV.red color to plot the FOV boudaries
#' @param feature.plot TRUE to plot the features
#' @param profbins number of bins of the surface brightness profile
#' @param xi_FOV FOV position along the x axis in units of rh
#' @param alpha_FOV FOV size along the x axis in units of rh
#' @param beta_FOV FOV size along the y axis in units of rh
#' @param miscentering amount of miscentering in units of rh
#' @param min_visible_type minimum star type that is considered visible, leave it set to -1 unless you know what you are doing
#' @param max_visible_type maximum star type that is considered visible, 3 supposedly includes only MS stars, 5 (or was it 6?) includes Abbas Askar's favourite stars, 10 was my initial choice
#' @keywords features
#' @export
#' @examples
#' generate_features(...)

generate_features <- function(path="./", seed = 37, diagno.on = FALSE, cluster.plot = FALSE, incomplete.gray = "#00334D34", complete.black = "#12031A", FOV.red = "#A05100", feature.plot = FALSE, profbins = 4, xi_FOV = 0, alpha_FOV = 0.5, beta_FOV = 0.5, miscentering = 0.0, min_visible_type = -1, max_visible_type = 5)
{
    if(!endsWith(path, "/")) path <- paste(path, "/", sep = "")
    ext_snap <- try(read_ext_snap(path=path))
    if(!anyNA(ext_snap))
    {
        snap <- select_visibles(ext_snap, min_visible_type, max_visible_type) #directly throws away invisible star types
        r <- snap$V2 #radius of the star (only this is known in a montecarlo)
        m <- rep(1, length(r)) #mass of the star
        L <- snap$V12 #luminosity of the star

        rr <- randomize_radii(r)
        x <- rr$x
        y <- rr$y

        #projected radius
        R2D <- sqrt(x*x + y*y)
        R2Dcut <- median(R2D)
        x <- rr$x + miscentering*R2Dcut #intentionally miscentering the cluster
        Lcut <- 0.5 #parameter for the completeness function... lower it to increase completeness

        #COMPLETENESS---------
        #select stars with a probability given by their completeness
        gamma <- runif(length(R2D))
        complete <- !(gamma > completeness(R2D, R2Dcut, L, Lcut))

        R2D <- R2D[complete]
        rh <- median(R2D)

        if(cluster.plot) plot_cluster(x, y, rh, complete, paste(path, "FOV_and_completeness.pdf", sep=""), incomplete.gray, complete.black, FOV.red)

        x <- x[complete]
        y <- y[complete]
        m <- m[complete]

        #FIELD OF VIEW---------------------------------------------------
        #the field of view is alway symmetric in y and centered on xi_FOV*rh > 0
        #this is because x and y are generated from r isotropically
        #so there is no loss of generality by choosing the FOV this way
        #The FOV is a rectangle of semisides alpha_FOV*rh (along x) and beta_FOV*rh (along y)
        wFOV <- FOVselect(x, y, xi_FOV, alpha_FOV, beta_FOV, rh)
        x <- x[wFOV]
        y <- y[wFOV]
        m <- m[wFOV]
        R2D <- R2D[wFOV]

        # write the features x, y, r, m into a new file
        if (diagno.on) write.table(data.frame(x, y, R2D, m), paste(path, "snap.pro", sep = ""), quote = F, row.names = F)

        fea <- profile(R2D/rh, m, profbins, FALSE)
        warning(paste("rh is ", rh, "; length(R2D) is ", length(R2D), "; length(m) is ", length(m),  sep = ""))
        fea <- c(truncate_path(path), as.vector(fea))

        return(t(fea))
    } else {
        warning(paste("Cannot generate features for ", path, sep = ""))
        fea <- rep(NA, 2*profbins)
        fea <- c(truncate_path(path), as.vector(fea))
        return(t(fea))
    }
}

