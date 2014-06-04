package lila.blog

import scala.concurrent.duration._

final class LastPostCache(api: BlogApi, ttl: Duration) {

  private val cache = lila.memo.MixedCache.single[List[MiniPost]](
    api.prismicApi flatMap { prismic =>
      api.recent(prismic, none, 3) map (_.results flatMap MiniPost.fromDocument)
    },
    timeToLive = ttl,
    default = Nil,
    awaitTime = 1.millisecond)

  def apply = cache get true

  private[blog] def clear {
    cache invalidate true
  }
}
