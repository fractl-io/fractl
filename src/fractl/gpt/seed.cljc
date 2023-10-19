(ns fractl.gpt.seed)

(def full-conversation
  [{:role "system" :content "You are the fractl assistant."},
   {:role "user" :content "Let's create an e-commerce application similar to Etsy. Define an entity for 'User'."}
   {:role "assistant" :content "(entity :Etsy.User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email, :Password :Password})"}
   {:role "user" :content "Good. Define an entity for 'Product'."}
   {:role "assistant" :content "(entity :Etsy.Product {:Id :Identity, :Title :String, :Description :String, :Price :Decimal, :Stock :Int, :Seller {:ref :Etsy.User.Id}})"}
   {:role "user" :content "How can we represent categories for products?"}
   {:role "assistant" :content "(entity :Etsy.Category {:Id :Identity, :Name :String}) (relationship :Etsy.ProductCategory {:meta {:between [:Etsy.Product :Etsy.Category]}})"}
   {:role "user" :content "What if we want to represent product images?"}
   {:role "assistant" :content "(entity :Etsy.Image {:Id :Identity, :URL :String}) (relationship :Etsy.ProductImage {:meta {:between [:Etsy.Product :Etsy.Image]}})"}
   {:role "user" :content "How can users place orders for products?"}
   {:role "assistant" :content "(entity :Etsy.Order {:Id :Identity, :User {:ref :Etsy.User.Id}, :Product {:ref :Etsy.Product.Id}, :Quantity :Int, :TotalAmount :Decimal, :OrderDate :Now})"}
   {:role "user" :content "What if we want to keep track of user reviews for products?"}
   {:role "assistant" :content "(entity :Etsy.Review {:Id :Identity, :User {:ref :Etsy.User.Id}, :Product {:ref :Etsy.Product.Id}, :Rating :Int, :Comment :Text, :Date :Now})"}
   {:role "user" :content "How can we model user preferences, like favorite products?"}
   {:role "assistant" :content "(relationship :Etsy.FavoriteProduct {:meta {:between [:Etsy.User :Etsy.Product]}})"}
   {:role "user" :content "Define a cart concept to hold selected products before checkout."}
   {:role "assistant" :content "(entity :Etsy.Cart {:Id :Identity, :User {:ref :Etsy.User.Id}, :Contents {:listof :Etsy.Product.Id}})"}
   {:role "user" :content "Great! Now let's consider handling payment information."}
   {:role "assistant" :content "(entity :Etsy.PaymentMethod {:Id :Identity, :User {:ref :Etsy.User.Id}, :CardNumber :CreditCard, :ExpirationDate :Date, :CVV :String, :BillingAddress {:type :Etsy.Address}})"}
   {:role "user" :content "What about shipping addresses?"}
   {:role "assistant" :content "(entity :Etsy.Address {:Id :Identity, :User {:ref :Etsy.User.Id}, :Street :String, :City :String, :State :String, :Zip :String})"}
   {:role "user" :content "Now, how can we handle order shipments?"}
   {:role "assistant" :content "(entity :Etsy.Shipment {:Id :Identity, :Order {:ref :Etsy.Order.Id}, :ShippingAddress {:ref :Etsy.Address.Id}, :ShippedDate :Now})"}
   {:role "user" :content "What if we want to track seller information and ratings?"}
   {:role "assistant" :content "(entity :Etsy.SellerInfo {:Id :Identity, :User {:ref :Etsy.User.Id}, :Rating :Decimal, :Description :Text})"}
   {:role "user" :content "Let's group these definitions under a 'Core' component."}
   {:role "assistant" :content "(component :Etsy.Core) (entity :Etsy.User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email, :Password :Password}) (entity :Etsy.Product {:Id :Identity, :Title :String, :Description :String, :Price :Decimal, :Stock :Int, :Seller {:ref :Etsy.User.Id}}) (entity :Etsy.Category {:Id :Identity, :Name :String}) (relationship :Etsy.ProductCategory {:meta {:between [:Etsy.Product :Etsy.Category]}}) (entity :Etsy.Image {:Id :Identity, :URL :String}) (relationship :Etsy.ProductImage {:meta {:between [:Etsy.Product :Etsy.Image]}}) (entity :Etsy.Order {:Id :Identity, :User {:ref :Etsy.User.Id}, :Product {:ref :Etsy.Product.Id}, :Quantity :Int, :TotalAmount :Decimal, :OrderDate :Now}) (entity :Etsy.Review {:Id :Identity, :User {:ref :Etsy.User.Id}, :Product {:ref :Etsy.Product.Id}, :Rating :Int, :Comment :Text, :Date :Now}) (relationship :Etsy.FavoriteProduct {:meta {:between [:Etsy.User :Etsy.Product]}}) (entity :Etsy.Cart {:Id :Identity, :User {:ref :Etsy.User.Id}, :Contents {:listof :Etsy.Product.Id}}) (entity :Etsy.PaymentMethod {:Id :Identity, :User {:ref :Etsy.User.Id}, :CardNumber :CreditCard, :ExpirationDate :Date, :CVV :String, :BillingAddress {:type :Etsy.Address}}) (entity :Etsy.Address {:Id :Identity, :User {:ref :Etsy.User.Id}, :Street :String, :City :String, :State :String, :Zip :String}) (entity :Etsy.Shipment {:Id :Identity, :Order {:ref :Etsy.Order.Id}, :ShippingAddress {:ref :Etsy.Address.Id}, :ShippedDate :Now}) (entity :Etsy.SellerInfo {:Id :Identity, :User {:ref :Etsy.User.Id}, :Rating :Decimal, :Description :Text})"}
   {:role "user" :content "How can we handle product reviews and ratings?"}
   {:role "assistant" :content "(entity :Etsy.ProductRating {:Id :Identity, :Product {:ref :Etsy.Product.Id}, :User {:ref :Etsy.User.Id}, :Rating :Decimal, :Review :Text, :Date :Now})"}
   {:role "user" :content "Great! Let's create a report for the top-rated products."}
   {:role "assistant" :content "(dataflow :Etsy.TopRatedProducts {:Etsy.ProductRating? {:order-by [[:Rating :desc]]}})"}
   {:role "user" :content "Show me a sample invocation of the :TopRatedProducts dataflow."}
   {:role "assistant" :content "{:Etsy.TopRatedProducts {}}"}
   {:role "user" :content "Define a dataflow to retrieve unread notifications for a user."}
   {:role "assistant" :content "(dataflow :Etsy.UnreadNotifications {:Etsy.Notification? {:where [:and [:= :User :Etsy.UnreadNotifications.User] [:= :Read false]]}})"}
   {:role "user" :content "How can we model a discount or coupon system?"}
   {:role "assistant" :content "(entity :Etsy.Discount {:Id :Identity, :Code :String, :Description :Text, :DiscountAmount :Decimal, :ValidFrom :DateTime, :ValidTo :DateTime})"}
   {:role "user" :content "What's the best way to associate discounts with orders?"}
   {:role "assistant" :content "(relationship :Etsy.OrderDiscount {:meta {:between [:Etsy.Order :Etsy.Discount]}})"}
   {:role "user" :content "Define a component to handle payment transactions."}
   {:role "assistant" :content "(component :Etsy.Payment) (entity :Etsy.Payment.Transaction {:Id :Identity, :User {:ref :Etsy.User.Id}, :Amount :Decimal, :TransactionDate :Now}) (relationship :Etsy.PaymentHistory {:meta {:between [:Etsy.User :Etsy.Payment.Transaction]}})"}
   {:role "user" :content "How can we track the popularity of products based on views or purchases?"}
   {:role "assistant" :content "(entity :Etsy.Popularity {:Id :Identity, :Product {:ref :Etsy.Product.Id}, :Views :Int, :Purchases :Int})"}
   {:role "user" :content "Create a dataflow to find popular products based on views."}
   {:role "assistant" :content "(dataflow :Etsy.PopularProductsByViews {:Etsy.Popularity? {:order-by [[:Views :desc]]}})"}
   {:role "user" :content "Can you show a sample invocation for the :PopularProductsByViews dataflow?"}
   {:role "assistant" :content "{:Etsy.PopularProductsByViews {}}"}
   {:role "user" :content "How can we track stock changes for products?"}
   {:role "assistant" :content "(entity :Etsy.StockChange {:Id :Identity, :Product {:ref :Etsy.Product.Id}, :ChangeType :String, :ChangeAmount :Int, :ChangeDate :Now})"}])
