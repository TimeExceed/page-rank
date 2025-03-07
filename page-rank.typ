#import "@preview/algo:0.3.4": algo, i, d, comment, code
#import "@preview/ctheorems:1.1.3": *
#show: thmrules.with(qed-symbol: $square$)

#let theorem = thmbox("theorem", "Theorem", fill: rgb("#eeffee"))
#let proposition = thmbox("proposition", "Proposition", fill: rgb("#eeffee"))
#let corollary = thmplain(
  "corollary",
  "Corollary",
  base: "theorem",
  titlefmt: strong
)
#let definition = thmbox("definition", "Definition", inset: (x: 1.2em, top: 1em))
#let example = thmplain("example", "Example").with(numbering: none)
#let proof = thmproof("proof", "Proof")

#set heading(numbering: "1.1.")

#set document(
  title: [PageRank 和 Graph Expander],
  author: "陶大",
)

#context [
  #align(center, text(17pt)[
    *#document.title*
  ])
]

本文的内容取自#cite(<wikipedia>)、#cite(<local-partition>)和#cite(<andersen2006local>)。
符号杂糅，并不单取一篇。

= Theory

== Graph Expander & Conductance

#let Vol = [$op("Vol")$]

#definition[
  给定一个无向图 $G=(V, E)$和一个节点子集 $S$。
  我们定义
  - 容量：$Vol(S) = sum_(v in S) d(v)$，其中 $d(v)$ 是 $v$ 的度。
  - 割：$partial S={(u,v) in E | u in S, v in.not S}$
]

#definition("Edge Expansion")[
  给定一个无向图 $G=(V, E)$。
  我们定义其 *奇格常数* (*Cheeger constant*) 或 *等周数* (*isoperimetric number*)：
  $
    h(G) = min_(emptyset != S subset V) (|partial S|) / (min{|S|, |V without S|})
  $
  使得 $h(dot.c)$ 取到最小值的 $S$ 被称为一个(edge) expander。
]

Expander试图找出一个高内聚且与外部低耦合的一个子图。
不同的“内聚”和“耦合”的定义可以得出（稍稍）不同的子图。
常见者，除了这里给出的 edge expansion、还有 vertex expansion 和 spectral expansion。

#definition("Conductance")[
  给定一个无向图 $G=(V, E)$。
  我们定义其传导性 (conductance) 为
  $
    Phi(S) &= (|partial S|) / (min{Vol(S), Vol(V without S)})\
    Phi(G) &= min_(emptyset != S subset V) Phi(S)
  $
]

Conductance 起源于 Mark Jerrum 和 Alistair Sinclair 在1988年对随机游走的研究。
Conductance 不仅在形式上和奇格常数非常相似，它们在直观上也是相同的。

== Page Rank

#let pr = [$op("pr")$]
#let apr = [$op("apr")$]

设有一个有向图 $G=(V,E)$。
给定一个点上的概率密度 $arrow(s)$ 和衰退系数(damping factor) $alpha in (0,1]$，
那么其每个点的page rank定义为如下不动点：

$ arrow(p) = alpha arrow(s) + (1 - alpha) W arrow(p) $

其中
- $arrow(p)$ 是各点page rank所构成的向量。
  $arrow(p)$ 是 $arrow(s)$ 和 $alpha$的函数，因此也可以写成 $pr (alpha, arrow(s))$。
- $arrow(s)$ 是起点的概率密度。
- $W$ 是图 $G$ 的概率矩阵，即，$W_(u v)$ 表示若当前处于 $u$，则下一步走到 $v$ 的概率。
  显然，
  $ cases(
    sum_u W_(u v) = 1 quad forall v in V,
    (u,v) in.not E arrow.r.double W_(u v) = 0,
  ) $

我们简记 $f(S)=sum_(v in S) f(v)$ 若 $f(dot.c)$ 是任意定义在节点上的函数且 $S$ 是任意节点子集。

直观来讲，$arrow(p)$ 描述了这么一个过程的结果：
若以 $arrow(s)$ 为概率选择一个起点，以 $W$ 为概率在图中随机游走。
每走一步都会带有 $alpha$ 的衰退。
那么稳定时各点的概率分布即为 $arrow(p)$。

通常我们会关心这样一些特殊的起点设置：
- 均匀分布。即，$arrow(s)=1/n arrow(1)$。
  通常搜索引擎会比较关心这种分布。
- 个性化分布。即 $arrow(s)$ 只在 $V$ 的一个真子集上均匀分布。
- 单一起点。对于这种场景，我们简记向量
  $ chi_v = cases(
    1 quad "if" x = v,
    0 quad "o.w."
  ) $

#proposition[
  对于任意 $arrow(s)$ 和任意衰退系数 $alpha in (0, 1]$，都有唯一的 $pr (alpha, arrow(s))$。
]

#proposition[
  对于任意衰退系数 $alpha in (0, 1]$，都存在矩阵 $R_alpha$，使得
  $ pr (alpha, arrow(s)) = R_alpha arrow(s) $
]<linear-transformation>

#definition[
  我们称 $arrow(p)$ 是 $pr(alpha, arrow(s))$ 的一个近似，如果存在 $arrow(r)$，使得
  $ arrow(p) + pr(alpha, arrow(r)) = pr(alpha, arrow(s)) $
  其中 $arrow(r)$ 称为残差向量 (residual vector)。

  更进一步，如果存在 $epsilon$ 使得 $arrow(r)$ 满足
  $ 0 lt.eq.slant arrow(r)(v) lt.eq.slant epsilon d(v) quad forall v in V $
  我们称 $arrow(p)$ 是一个 $epsilon$ 近似 ($epsilon$-approximation)。

  我们记 $apr(alpha, arrow(s), arrow(r)) = pr(alpha, arrow(s)) - pr(alpha, arrow(r))$。
]

由 @linear-transformation 可知，$apr(alpha, arrow(s), arrow(r))=pr(alpha, arrow(s) - arrow(r))$，
如果我们忽略 $arrow(s) - arrow(r)$ 可能不是一个概率密度函数的话。

#theorem[
  设 $arrow(p)=apr(alpha, arrow(s), arrow(r))$ 是一个近似PageRank。
  那么
  $ arrow(p) lt.eq.slant alpha arrow(s) + (1 - alpha) W arrow(p) $
]

#theorem[
  令 $m = Vol(V) / 2$。
  设 $arrow(p)$ 是一个近似PageRank，
  $S subset.eq V$
  且 $delta gt.eq.slant 2 / (sqrt(m))$ 是一个常数。
  若
  $ arrow(p)(S) - Vol(S) / Vol(V) gt delta $
  则
  $ min_(emptyset != S' subset.eq S) Phi (S') < sqrt((18 alpha ln m) / delta) $
]

= Algorithms

#algo(
  title: "ApproximatePageRank",
  parameters: ($v in V$, ),
)[
  let $arrow(p) = 0$\
  let $arrow(r)=chi_v$\
  let $Gamma={u in V | r(u) > epsilon d(u)}$\
  while $Gamma eq.not emptyset$:#i\
    pick any $u in Gamma$\
    let $(arrow(p)',arrow(r)')= #smallcaps[push] (u, arrow(p), arrow(r))$\
    update $arrow(p),arrow(r)$ by $arrow(p)',arrow(r)'$#d\
  return $arrow(p)$
]

#algo(
  title: "push",
  parameters: ($u in V$, $arrow(p)$, $arrow(r)$),
)[
  let $arrow(p)' = arrow(p)$\
  let $arrow(r)' = arrow(r)$\
  let $arrow(p)'(u) = arrow(p)(u) + alpha arrow(r)(u)$\
  let $arrow(r)'(u) = (1 - alpha) / 2 arrow(r)(u) $\
  for each $v$ s.t. $(u,v) in E$:#i\
    let $arrow(r)'(v) = arrow(r)(v) + (1 - alpha) / (2 d(u)) arrow(r)(u)$ #d\
  return $arrow(p)'$, $arrow(r)'$
]

#theorem[
  对于任意 $v in V$, $alpha in (0, 1]$ 和 $epsilon in (0, 1]$，
  #smallcaps[ApproximatePageRank] ($v$) 输出一个 $epsilon$-approximated PageRank $arrow(p)$，
  满足
  $ Vol(sup (arrow(p))) lt.eq.slant 2 / ((1 - alpha) epsilon) $
  运行时间是 $O( 1 / (epsilon alpha))$。
]

#definition[
  令 $arrow(p)$ 是一个PageRank。
  我们定义排序函数(ranking function) $q(u)=(arrow(p)(u)) / d(u), forall u in V$，
  以及其对应的降序 $pi$，即
  $ q(pi(1)) gt.eq.slant q(pi(2)) gt.eq.slant dots gt.eq.slant q(pi(|V|)) $
  对应此序，我们定义其前缀 $S_j={pi(1), pi(2), dots, pi(j)}$。
]

#proposition[
  令 $phi.alt in (0, 1)$ 是任意常数。
  若 $arrow(p)$ 是一个（近似）PageRank且 $q, pi$ 分别是其导出排序函数和序。
  则对于任意前缀 $S_j$ ，
  或者满足 $E(S_j, V without S_j) < 2 phi.alt Vol(S_j)$，
  或者存在 $k>j$ 使得
  $ cases(
    Vol (S_k) gt.eq.slant (1 + phi.alt) Vol(S_j),
    alpha / phi.alt Vol (S_j) gt.eq.slant q(pi(j)) - q(pi(k))
  ) $
]

#proposition("Integration Lemma")[
  对于任意概率密度 $arrow(p)$ 以及导出的排序函数 $q$ 、序 $pi$和前缀 $S_i$，
  都存在下标 $i$，满足
  $ q(pi(i)) gt.eq.slant 1/ (H(Vol(V)) Vol(S_i)) $
  其中 $H(dot.c)$ 是harmonic number，即，$H(n)=sum_(k=1)^n 1/k$。
]

#proposition("Approximation Error Lemma")[
  令 $arrow(p)$ 是一个PageRank，以及其导出的排序函数$q$、序 $pi$ 和前缀 $S_i$。
  令 $arrow(p)'$ 是 $arrow(p)$ 的一个 $epsilon$-approximation。
  其相应的排序函数为 $q'$，序为 $pi'$，前缀为 $S'_i$。
  那么对于任意 $i in pi$，存在 $i' in pi'$，使得
  $
    epsilon &gt.eq.slant q(i) - q'(i')\
    Vol(S_(i')) &gt.eq.slant Vol(S_i)
  $
]

#algo(
  title: "LocalPartition",
  parameters: ($v in V$, $phi.alt in (0, 1/3)$, $x in [0, Vol(V)]$),
  comment-prefix: [#sym.triangle.stroked.r ],
)[
  #comment(inline:true)[$v$ 是起点]\
  #comment(inline:true)[$phi.alt$ 是目标conductance]\
  #comment(inline:true)[$x$ 是目标容量]\
  let $gamma = H(Vol(V))$ #comment[$H(dot.c)$ 是 harmonic number]\
  let $alpha = (phi.alt ^ 2) / (8 gamma)$\
  let $epsilon = 1 / (2 gamma x)$\
  let $arrow(p)$ #sym.arrow.l #smallcaps[ApproximatePageRank] ($v$)\
  按照导出的排序函数 $q$ 重新排序节点，得到序 $pi$\
  let $k$ #sym.arrow.l the largest index $i$ st. $q(pi(i)) gt.eq.slant 1/ (2 gamma Vol(S_i))$#i\
    if no such $k$, halt and output "FAIL: NO STARTING INDEX" #d\
  repeat#i\
    if $(1 + phi.alt) Vol(S_k) > Vol(V)$ or $Vol(S_k) > Vol(sup(arrow(p)))$#i\
      output "FAIL NO CUT FOUND" and quit with $S_k$#d\
    let $k'$ #sym.arrow.l the smallest index $i$ st. $Vol(S_i) gt.eq.slant (1+phi.alt)Vol(S_k)$\
    if $alpha / (phi.alt Vol(S_k)) lt.eq.slant q(pi(k)) - q(pi(k'))$#i\
      quit with $S_k$#d\
    let $k$ #sym.arrow.l $k'$
]

#theorem[
  #smallcaps[LocalPartition] is $O((log^2 Vol(V)) / (phi.alt^2) x)$.
]

#theorem[
  给定 $v, phi.alt, x$。
  令$arrow(p)$、$q$、$pi$ 和 $S$ 为PageRank及其对应排序函数、序和前缀，
  $arrow(p)'$、$q'$、$pi'$ 和 $S'$ 为#smallcaps[LocalPartition]所计算的近似PageRank以及相应的排序函数、序和前缀。
  于是以下性质成立：
  + 存在下标 $k in pi$ 满足 $q(pi(k)) gt.eq.slant 1/gamma Vol(S_k)$。
    + 更进一步，若 $x gt.eq.slant Vol(S_k)$，算法总会找到一个初始的下标 $k_0$ 满足 $Vol(S'_(k_0)) gt.eq.slant Vol(S_k)$。
  + 设令 $x gt.eq.slant Vol(S_k)$。
    则算法将正常执行结束并输出一个集合 $D$，满足
    + $Phi(D) lt.eq.slant 3 phi.alt$
    + $Vol(S_k) lt.eq.slant Vol(D) lt.eq.slant 5/9 Vol(V)$
  + 更进一步，若存在集合 $C$ 满足
    $
      Vol(C) &lt.eq.slant 1/2 Vol(V)\
      Phi(C) &lt.eq.slant alpha / (80 gamma)\
      v &in C_alpha
    $
    其中$C_alpha={u in C | pr(alpha, u) (V minus C) lt.eq.slant (2Phi(C)) / alpha}$，
    则 $Vol(C inter D) gt.eq.slant 9 / 10 Vol(D)$。
]


#bibliography("bib.yaml")
