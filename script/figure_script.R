# figures for the forestinecology workshop

## building a tree
library(rpart)
library(rpart.plot)
library(ggplot2)

set.seed(123)
n <- 200
x1 <- runif(n, 0, 10)
x2 <- runif(n, 0, 10)
y <- ifelse(x1 + x2 + rnorm(n) > 10, "Class1", "Class2")
data <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

cart_model <- rpart(y ~ x1 + x2, data = data, method = "class",
                    control = rpart.control(cp = 0.01))

rpart.plot(cart_model, type = 3, extra = 101, fallen.leaves = TRUE)

g1 <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 4.2, linetype = "dashed") +
  labs(title = paste("Split 1")) +
  theme_minimal(base_size = 14)

g2 <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 4.2, linetype = "dashed") +
  geom_vline(xintercept = 2.7, linetype = "dashed") +
  labs(title = paste("Split 2")) +
  theme_minimal(base_size = 14)

g3 <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 4.2, linetype = "dashed") +
  geom_vline(xintercept = 2.7, linetype = "dashed") +
  geom_vline(xintercept = 6.7, linetype = "dashed") +
  labs(title = paste("Split 3")) +
  theme_minimal(base_size = 14)

g4 <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 4.2, linetype = "dashed") +
  geom_hline(yintercept = 7.9, linetype = "dashed") +
  geom_vline(xintercept = 2.7, linetype = "dashed") +
  geom_vline(xintercept = 6.7, linetype = "dashed") +
  labs(title = paste("Split 4")) +
  theme_minimal(base_size = 14)

g5 <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 4.2, linetype = "dashed") +
  geom_hline(yintercept = 7.9, linetype = "dashed") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_vline(xintercept = 2.7, linetype = "dashed") +
  geom_vline(xintercept = 6.7, linetype = "dashed") +
  labs(title = paste("Split 5")) +
  theme_minimal(base_size = 14)


plot_cart_splits <- function(model, data) {
  out <- list()
  frame <- model$frame
  splits <- model$splits
  split_nodes <- frame[frame$var != "<leaf>", ]
  
  split_steps <- list()
  
  # Get the row numbers (node IDs) to preserve the order
  node_ids <- as.numeric(rownames(split_nodes))
  
  for (i in seq_along(node_ids)) {
    node <- node_ids[i]
    var <- as.character(split_nodes$var[i])
    
    # Find split value for this node
    # Access the split value via model$frame$splits doesn't work, so:
    # Use the 'model$splits' and 'model$csplit' indirectly
    
    # Get the split value from 'model$frame$split' alternative: use 'model$splits' order
    # But 'model$splits' can have multiple entries per variable if multiple splits
    # So instead, we extract the split value from 'model$splits' by matching variable name *and* row order
    
    # Use 'split_nodes$split' value (hidden in 'split_nodes') — here's the trick:
    # Extract the actual split value for each split from the frame's rownames and match to where it happens in the tree
    split_val <- split_nodes$split[i]
    
    split_steps[[i]] <- list(var = var, val = split_val)
  }
  
  # Plot step by step
  for (i in seq_along(split_steps)) {
    g <- ggplot(data, aes(x = x1, y = x2, color = y)) +
      geom_point(alpha = 0.6, size = 2) +
      labs(title = paste("Step", i, ": Split on", split_steps[[i]]$var)) +
      theme_minimal(base_size = 14)
    
    for (j in 1:i) {
      s <- split_steps[[j]]
      if (s$var == "x1") {
        g <- g + geom_vline(xintercept = s$val, linetype = "dashed")
      } else if (s$var == "x2") {
        g <- g + geom_hline(yintercept = s$val, linetype = "dashed")
      }
    }
    out[[length(out) + 1]] <- g 
    
  }
  return(out)
}

gg <- plot_cart_splits(cart_model, data)


## other try
library(ggplot2)
library(DiagrammeR)
library(gridExtra)
library(patchwork)

set.seed(123)
X1 <- c(0.1, 0.3, 0.4, 0.2, 0.7, 0.8, 0.9, 0.5, 0.3, 0.6, 0.85, 0.8) 
X2 <- c(0.1, 0.2, 0.3, 0.4, 0.2, 0.4, 0.3, 0.3, 0.8, 0.9, 0.9, 0.6)
values <- c(1:12)

# Function to draw a split
plot_split <- function(X1, X2, values, split_var = 2, threshold = 0.5) {
  plot(X1, X2, xlim = c(0, 1), ylim = c(0, 1), usr = c(0, 1, 0, 1), 
       xlab = expression(X^{(1)}), ylab = expression(X^{(2)}),
       col = "white", asp = 1)
  text(X1, X2, labels = values, col = "blue")
  
  if (split_var == 1) {
    abline(v = threshold, col = "red", lwd = 2)
  } else if (split_var == 2) {
    abline(h = threshold, col = "red", lwd = 2)
  }
}

# Function to draw the tree split
draw_tree <- function(split_var = 2, threshold = 0.5) {
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  symbols(0.5, 0.8, circles = 0.05, inches = FALSE, add = TRUE)
  text(0.5, 0.8, "t1")
  
  symbols(0.3, 0.5, circles = 0.05, inches = FALSE, add = TRUE)
  text(0.3, 0.5, "t2")
  
  symbols(0.7, 0.5, circles = 0.05, inches = FALSE, add = TRUE)
  text(0.7, 0.5, "t3")
  
  arrows(0.48, 0.75, 0.32, 0.55, length = 0.1)
  arrows(0.52, 0.75, 0.68, 0.55, length = 0.1)
  
  # Text above
  text(0.5, 0.95, labels = paste0("Découpage (X", split_var, ", ", threshold, ")"), cex = 1.2)
  text(0.25, 0.65, bquote(X^{(.(split_var))} <= .(threshold)))
  text(0.75, 0.65, bquote(X^{(.(split_var))} > .(threshold)))
}

# Plot 1: Horizontal split at X2 = 0.5
par(mfrow = c(2, 2))
plot_split(X1, X2, values, split_var = 2, threshold = 0.5)
draw_tree(split_var = 2, threshold = 0.5)

# Plot 2: Vertical split at X1 = 0.65
plot_split(X1, X2, values, split_var = 1, threshold = 0.65)
draw_tree(split_var = 1, threshold = 0.65)



g1 <- ggplot(data.frame(x1=X1, x2=X2), aes(x=x1, y=x2)) +
  geom_text(aes(label = 1:12), size = 10) +
  theme_minimal() +
  geom_vline(xintercept = 0.65, color="red") +
  labs(title = "Option 1") +
  theme(text = element_text(size = 20))

g2 <- ggplot(data.frame(x1=X1, x2=X2), aes(x=x1, y=x2)) +
  geom_text(aes(label = 1:12), size = 10) +
  theme_minimal() +
  geom_hline(yintercept = 0.5, color="red") +
  labs(title ="Option 2") +
  theme(text = element_text(size = 20))

ggsave("LIF/Presentation/ecostat_2025/split_ex.png", g1 + plot_spacer() + g2 + plot_layout(widths = c(4, -1, 4)),
       width = 16, height = 9)


g1 <- ggplot(data.frame(x1=X1, x2=X2), aes(x=x1, y=x2)) +
  geom_point(aes(color = 1:12 < 9), size = 10) +
  theme_minimal() +
  geom_vline(xintercept = 0.65, color="red") +
  labs(title = "Option 1") +
  theme(text = element_text(size = 20)) +
  guides(color = "none")

g2 <- ggplot(data.frame(x1=X1, x2=X2), aes(x=x1, y=x2)) +
  geom_point(aes(color = 1:12 < 9), size = 10) +
  theme_minimal() +
  geom_hline(yintercept = 0.5, color="red") +
  labs(title ="Option 2") +
  theme(text = element_text(size = 20)) +
  guides(color = "none")

ggsave("LIF/Presentation/ecostat_2025/splitcat_ex.png", g1 + plot_spacer() + g2 + plot_layout(widths = c(4, -1, 4)),
       width = 16, height = 9)




g1 <- ggplot(data.frame(x1=X1, x2=X2), aes(x=x1, y=x2)) +
  geom_text(aes(label = c(1, 2, 1, 2, 5, 4, 6, 5, 8, 11, 12, 10)),
            size = 10) +
  theme_minimal() +
  geom_hline(yintercept = 0.5, color="red", size = 2) +
  geom_segment(x = 0.45, xend=0.45, y = 0, yend =0.5, color="orange", size = 2) +
  geom_segment(x = 0.4, xend=0.4, y = 0.5, yend =1, color="green", size = 2) +
  theme(text = element_text(size = 20))

ggsave("LIF/Presentation/ecostat_2025/spacesplit_ex.png", g1,
       width = 6, height = 6)

plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
symbols(0.5, 0.8, circles = 0.05, inches = FALSE, add = TRUE)
text(0.5, 0.8, "t1")

symbols(0.3, 0.5, circles = 0.05, inches = FALSE, add = TRUE)
text(0.3, 0.5, "t2")

symbols(0.7, 0.5, circles = 0.05, inches = FALSE, add = TRUE)
text(0.7, 0.5, "t3")

arrows(0.48, 0.75, 0.32, 0.55, length = 0.1)
arrows(0.52, 0.75, 0.68, 0.55, length = 0.1)

text(0.25, 0.65, "X2 < 0.5", col = "red")
text(0.75, 0.65, "X2 >= 0.5", col = "red")

symbols(0.1, 0.1, circles = 0.05, inches = FALSE, add = TRUE)
text(0.1, 0.1, "t4")

symbols(0.4, 0.1, circles = 0.05, inches = FALSE, add = TRUE)
text(0.4, 0.1, "t5")

arrows(0.28, 0.45, 0.12, 0.15, length = 0.1)
arrows(0.32, 0.45, 0.4, 0.15, length = 0.1)

text(0.05, 0.25, "X1 < 0.45", col = "orange")
text(0.45, 0.25, "X1 >= 0.45", col ="orange")

symbols(0.6, 0.1, circles = 0.05, inches = FALSE, add = TRUE)
text(0.6, 0.1, "t6")

symbols(0.8, 0.1, circles = 0.05, inches = FALSE, add = TRUE)
text(0.8, 0.1, "t7")

arrows(0.72, 0.45, 0.82, 0.15, length = 0.1)
arrows(0.68, 0.45, 0.6, 0.15, length = 0.1)

text(0.6, 0.3, "X1 <\n0.4", col = "green")
text(0.85, 0.25, "X1 >= 0.4", col = "green")

ggsave("LIF/Presentation/ecostat_2025/figures/")

## prediction
g11 <- g1 + geom_text(label = "X", x = 0.6, y = 0.2, size = 10, color = "blue") +
  geom_rect(xmin = 0.45, xmax = 1 , ymin = 0, ymax = 0.5, fill = "lightblue", alpha = 0.05)

ggsave("LIF/Presentation/ecostat_2025/figures/pred_continu.png", g11, height = 6, width = 6)



g12 <- ggplot(data.frame(x1=X1, x2=X2, type = c(1, 1, 1, 1, 2, 2, 1, 2, 3, 4, 1, 4)),
       aes(x=x1, y=x2, color = factor(type))) +
  geom_point(size = 10) +
  theme_minimal() +
  geom_hline(yintercept = 0.5,  size = 2) +
  geom_segment(x = 0.45, xend=0.45, y = 0, yend =0.5,  size = 2, color = "black") +
  geom_segment(x = 0.4, xend=0.4, y = 0.5, yend = 1,  size = 2, color = "black") +
  theme(text = element_text(size = 20)) +
  guides(color="none") +
  geom_text(label = "X", x = 0.6, y = 0.2, size = 10, color = "blue") +
  geom_rect(xmin = 0.45, xmax = 1 , ymin = 0, ymax = 0.5, fill = "lightblue", alpha = 0.05)

ggsave("LIF/Presentation/ecostat_2025/figures/pred_discrete.png", g12, height = 6, width = 6)

## marchelike behavior of CART tree
x <- runif(100, -3.14, 3.14)
y <- sin(x) + rnorm(100, sd = 0.1)
dat <- data.frame(x = x, y = y)
mm <- rpart(y~x, data = dat)
newdat <- data.frame(x = seq(-3.14, 3.14, length.out = 100))

plot(x, y)
lines(newdat$x, predict(mm, newdat), col = "blue")

library(ipred)
mb <- bagging(y~x, data = dat)
lines(newdat$x, predict(mb, newdat), col = "orange")
lines(newdat$x, sin(newdat$x), col = "green", lty = 2)


x <- 0.3 * rnorm(100, mean = -2) + 0.7 * rnorm(100, mean = 5)
dd <- data.frame(x = x)
gd <- ggplot(data = dd, aes(x=x)) +
  geom_density() +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(x = "Y", y = "Probabilité",
       title = "Distribution conditionnelle",
       subtitle = "P(Y|X = x)")

ggsave("LIF/Presentation/ecostat_2025/figures/distrib.png", gd, width=8, height = 6)


x <- 0.3 * rnorm(100, mean = -2) + 0.7 * rnorm(100, mean = 5)
dd <- data.frame(x = x)
gd <- ggplot(data = dd, aes(x=x)) +
  geom_density() +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(x = "Production des arbres", y = "Probabilité",
       title = "Distribution conditionnelle",
       subtitle = "P(Production|Température = 10°C)")

ggsave("LIF/Presentation/ecostat_2025/figures/distrib_prod.png", gd, width=8, height = 6)

dat <- data.frame(x = c(0.3 * rnorm(100, mean = -2) + 0.7 * rnorm(100, mean = 5),
                        0.9 * rnorm(100, mean = -2) + 0.1 * rnorm(100, mean = 5),
                        0.5 * rnorm(100, mean = -2) + 0.5 * rnorm(100, mean = 5)),
                  temp = rep(c(0, 10, 20), each = 100))

gd <- ggplot(data = dat, aes(x=x, fill = factor(temp))) +
  geom_density(alpha=0.5) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(x = "Production des arbres", y = "Probabilité",
       title = "Distribution conditionnelle",
       subtitle = "P(Production|Température = x°C)") +
  guides(fill = guide_legend(title = "Temp."))

ggsave("LIF/Presentation/ecostat_2025/figures/distrib_prod_temp.png", gd, width=8, height = 6)


library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Plot
lincoln_weather$Month <- factor(lincoln_weather$Month, labels = 0:11)
gg <- ggplot(lincoln_weather, aes(x = `Mean Temperature [F]` / 50, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis() +
  labs(x = "Productivité", y = "Température") +
  coord_flip() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    text = element_text(size = 15)
  )
ggsave("LIF/Presentation/ecostat_2025/figures/distrib_prod_tempgrad.png", gg, width=8, height = 6)
