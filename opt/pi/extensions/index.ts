import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("clear", {
    description: "Alias for /new: start a new session",
    handler: async (_args, ctx) => {
      await ctx.waitForIdle();
      const result = await ctx.newSession();
      if (!result.cancelled) {
        ctx.ui.notify("Started a new session.", "info");
      }
    },
  });
}
