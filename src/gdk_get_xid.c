/* get the xid (can't find the function in haskell x11 bindings), works for gtk2 and gtk3 */
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

unsigned long get_xid(GtkWindow *window) {
	GdkWindow *gdk_window = gtk_widget_get_window(GTK_WIDGET(window));
	return GDK_WINDOW_XID(gdk_window);
}
