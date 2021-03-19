(ns fractl.store.sfdc.metadata-types)

;; Unsupported Metadata types:
;; https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_unsupported_types.htm
;; 
;; Metadata Coverage Report:
;; https://developer.salesforce.com/docs/metadata-coverage/51

(comment
  (def fractl-types (into {} (wsdl->fractl "resources/sf-metadata.wsdl")))
  (def fractl-code (emit-fractl :Sfdc/Metadata fractl-types))

  (defmacro make-fn [m]
    `(fn [& args#]
       (eval
        (cons '~m args#))))

  (defn binary-fractl-type?
    [f-type]
    (when (= (get-in fractl-types [f-type :type]) :record)
      (let [attrs-binary? (apply (make-fn or)
                                 (map (fn [t]
                                        (cond
                                          (= t :Kernel/Base64Binary)
                                          true

                                          (contains? fractl-base-types t)
                                          false

                                          (contains? fractl-types t)
                                          (binary-fractl-type? t)

                                          :else
                                          false))
                                      (vals (get-in fractl-types [f-type :attributes]))))
            extends-binary? (if-let [extends (keyword (get-in fractl-types [f-type :meta :extends]))]
                              (binary-fractl-type? extends)
                              false)]
        (or attrs-binary? extends-binary?))))

  (def meta-abstract-names
    (let [abstracts (into #{:Metadata} (filter #(not (nil? %)) (map (fn [[k v]] (keyword (get-in v [:meta :extends]))) fractl-types)))
          abstract-recs (select-keys fractl-types abstracts)]
      (loop [abstract #{:Metadata}]
        (let [derived (into #{} (keys (into {} (filter (fn [[k v]] (contains? abstract (keyword (get-in v [:meta :extends])))) abstract-recs))))
              new-abstracts (into abstract derived)]
          (if (= abstract new-abstracts)
            abstract
            (recur new-abstracts))))))

  (def meta-abstract-deps (filter (fn [[k v]] (contains? meta-abstract-names (keyword (get-in v [:meta :extends])))) fractl-types))
  (def meta-abstract-dep-names (into #{} (keys meta-abstract-deps)))
  (def meta-derivatives (clojure.set/difference meta-abstract-dep-names meta-abstract-names))
  (clojure.pprint/pprint (sort (distinct (vec meta-derivatives))) (clojure.java.io/writer "meta-derivatives.txt"))


  (def meta-derivatives-with-bin-attrs
    (filter binary-fractl-type? meta-derivatives))
  )

(def type-names
  ;; Same as `meta-derivatives` above
  [:AIReplyRecommendationsSettings
   :AccessControlPolicy
   :AccountInsightsSettings
   :AccountIntelligenceSettings
   :AccountRelationshipShareRule
   :AccountSettings
   :ActionLinkGroupTemplate
   :ActionPlanTemplate
   :ActionsSettings
   :ActivitiesSettings
   :AddressSettings
   :AnalyticSnapshot
   :AnalyticsSettings
   :AnimationRule
   :ApexClass
   :ApexComponent
   :ApexEmailNotifications
   :ApexPage
   :ApexSettings
   :ApexTestSuite
   :ApexTrigger
   :AppAnalyticsSettings
   :AppExperienceSettings
   :AppMenu
   :AppointmentSchedulingPolicy
   :ApprovalProcess
   :ArchiveSettings
   :AssignmentRule
   :AssignmentRules
   :Audience
   :AuraDefinitionBundle
   :AuthProvider
   :AutoResponseRule
   :AutoResponseRules
   :AutomatedContactsSettings
   :BlacklistedConsumer
   :BlockchainSettings
   :BotSettings
   :BrandingSet
   :BriefcaseDefinition
   ;; :BusinessHoursEntry ;; Errors on retrieve and not listed in the metadata coverage report
   :BusinessHoursSettings
   :BusinessProcess
   :BusinessProcessGroup
   :CMSConnectSource
   :CallCenter
   :CallCoachingMediaProvider
   :CampaignInfluenceModel
   :CampaignSettings
   :CanvasMetadata
   :CareProviderSearchConfig
   :CareRequestConfiguration
   :CareSystemFieldMapping
   :CaseClassificationSettings
   :CaseSettings
   :CaseSubjectParticle
   :Certificate
   :ChannelLayout
   :ChannelObjectLinkingRule
   :ChatterAnswersSettings
   :ChatterEmailsMDSettings
   :ChatterExtension
   :ChatterSettings
   :CleanDataService
   :CommandAction
   :CommunitiesSettings
   :Community
   :CommunityThemeDefinition
   :CompactLayout
   :CompanySettings
   :ConnectedApp
   :ConnectedAppSettings
   :ContentAsset
   :ContentSettings
   :ContractSettings
   :ConversationVendorFieldDef
   :ConversationVendorInfo
   :ConversationalIntelligenceSettings
   :CorsWhitelistOrigin
   :CspTrustedSite
   :CurrencySettings
   :CustomApplication
   :CustomApplicationComponent
   :CustomFeedFilter
   :CustomField
   :CustomHelpMenuSection
   :CustomIndex
   :CustomLabel
   :CustomLabels
   :CustomMetadata
   :CustomNotificationType
   :CustomObject
   :CustomObjectTranslation
   :CustomPageWebLink
   :CustomPermission
   :CustomSite
   :CustomTab
   :CustomerDataPlatformSettings
   :Dashboard
   :DashboardFolder
   :DataDotComSettings
   :DelegateGroup
   :DeploymentSettings
   :DevHubSettings
   :DiscoveryAIModel
   :DiscoveryGoal
   :DiscoverySettings
   :Document
   :DocumentChecklistSettings
   :DocumentFolder
   :DocumentType
   :DuplicateRule
   :DynamicTrigger
   :EACSettings
   :EclairGeoData
   :EinsteinAssistantSettings
   :EmailAdministrationSettings
   :EmailFolder
   :EmailIntegrationSettings
   :EmailServicesFunction
   :EmailTemplate
   :EmailTemplateFolder
   :EmailTemplateSettings
   :EmbeddedServiceBranding
   :EmbeddedServiceConfig
   :EmbeddedServiceFlowConfig
   :EmbeddedServiceLiveAgent
   :EmbeddedServiceMenuSettings
   :EmployeeUserSettings
   :EncryptionKeySettings
   :EnhancedNotesSettings
   :EntitlementProcess
   :EntitlementSettings
   :EntitlementTemplate
   :EntityImplements
   :EscalationRule
   :EscalationRules
   :EssentialsSettings
   :EventSettings
   :ExperienceBundle
   :ExperienceBundleSettings
   :ExternalDataSource
   :ExternalServiceRegistration
   :ExternalServicesSettings
   :FieldServiceMobileExtension
   :FieldServiceSettings
   :FieldSet
   :FileUploadAndDownloadSecuritySettings
   :FilesConnectSettings
   :FlexiPage
   :FlowCategory
   :FlowDefinition
   :FlowSettings
   :ForecastingObjectListSettings
   :ForecastingSettings
   :FormulaSettings
   :GatewayProviderPaymentMethodType
   :GlobalValueSet
   :GlobalValueSetTranslation
   :GoogleAppsSettings
   :Group
   :HighVelocitySalesSettings
   :HomePageComponent
   :HomePageLayout
   :IdeasSettings
   :IframeWhiteListUrlSettings
   :InboundCertificate
   :InboundNetworkConnection
   :Index
   :IndustriesManufacturingSettings
   :IndustriesSettings
   :InstalledPackage
   :InventorySettings
   :InvocableActionSettings
   :IoTSettings
   :IsvHammerSettings
   :KeywordList
   :KnowledgeSettings
   :LanguageSettings
   :Layout
   :LeadConfigSettings
   :LeadConvertSettings
   :Letterhead
   :LicenseDefinition
   :LightningBolt
   :LightningComponentBundle
   :LightningExperienceSettings
   :LightningExperienceTheme
   :LightningMessageChannel
   :LightningOnboardingConfig
   :ListView
   :LiveAgentSettings
   :LiveChatAgentConfig
   :LiveChatButton
   :LiveChatDeployment
   :LiveChatSensitiveDataRule
   :LiveMessageSettings
   :MacroSettings
   :ManagedContentType
   :ManagedTopic
   :ManagedTopics
   :MapsAndLocationSettings
   :MatchingRule
   :MatchingRules
   :MeetingsSettings
   :MilestoneType
   :MlDomain
   :MobileApplicationDetail
   :MobileSettings
   :ModerationRule
   :MutingPermissionSet
   :MyDomainDiscoverableLogin
   :MyDomainSettings
   :NameSettings
   :NamedCredential
   :NetworkBranding
   :NotificationTypeConfig
   :NotificationsSettings
   :OauthCustomScope
   :ObjectLinkingSettings
   :OmniChannelSettings
   :OpportunityInsightsSettings
   :OpportunityScoreSettings
   :OpportunitySettings
   :Orchestration
   :OrchestrationContext
   :OrderManagementSettings
   :OrderSettings
   :OrgSettings
   :OutboundNetworkConnection
   ;; :Package ;; Errors out on retrieve and doesn't have API support per metadata coverage report
   :PardotEinsteinSettings
   :PardotSettings
   :ParticipantRole
   :PartyDataModelSettings
   :PathAssistant
   :PathAssistantSettings
   :PaymentGatewayProvider
   :PermissionSetGroup
   :PicklistSettings
   ;; :PicklistValue ;; Errors out on retrieve
   :PlatformCachePartition
   :PlatformEncryptionSettings
   :PlatformEventChannel
   :PlatformEventChannelMember
   :PlatformEventSubscriberConfig
   :Portal
   :PortalsSettings
   :PostTemplate
   :PredictionBuilderSettings
   :PresenceDeclineReason
   :PresenceUserConfig
   :PrivacySettings
   :ProductSettings
   :Profile
   :ProfilePasswordPolicy
   :ProfileSessionSetting
   :Prompt
   :Queue
   :QueueRoutingConfig
   :QuickAction
   :QuickTextSettings
   :QuoteSettings
   :RealTimeEventSettings
   :RecommendationBuilderSettings
   :RecommendationStrategy
   :RecordActionDeployment
   :RecordPageSettings
   :RecordType
   :RedirectWhitelistUrl
   :RemoteSiteSetting
   :ReportFolder
   :RestrictionRule
   :RetailExecutionSettings
   :Role
   :SalesWorkQueueSettings
   :SamlSsoConfig
   :SchemaSettings
   :Scontrol
   :SearchSettings
   :SecuritySettings
   :ServiceAISetupDefinition
   :ServiceAISetupField
   :ServiceChannel
   :ServiceCloudVoiceSettings
   :ServicePresenceStatus
   :ServiceSetupAssistantSettings
   :SharingCriteriaRule
   :SharingGuestRule
   :SharingReason
   :SharingRules
   :SharingSet
   :SharingSettings
   :SharingTerritoryRule
   :SiteDotCom
   :SiteSettings
   :Skill
   :SocialCustomerServiceSettings
   :SocialProfileSettings
   :SourceTrackingSettings
   :StandardValue
   :StandardValueSet
   :StandardValueSetTranslation
   :StaticResource
   :SurveySettings
   :SynonymDictionary
   :SystemNotificationSettings
   :Territory
   :Territory2
   :Territory2Model
   :Territory2Rule
   :Territory2Settings
   :Territory2Type
   :TimeSheetTemplate
   :TopicsForObjects
   :TrailheadSettings
   :TransactionSecurityPolicy
   :Translations
   :TrialOrgSettings
   :UIObjectRelationConfig
   :UiPlugin
   :UserAuthCertificate
   :UserCriteria
   :UserEngagementSettings
   :UserInterfaceSettings
   :UserManagementSettings
   :UserProvisioningConfig
   :ValidationRule
   :VoiceSettings
   :WaveApplication
   :WaveComponent
   :WaveDataflow
   :WaveDataset
   :WaveLens
   :WaveRecipe
   :WaveTemplateBundle
   :WaveXmd
   :WebLink
   :WebStoreTemplate
   :WebToXSettings
   :WorkDotComSettings
   :Workflow
   :WorkflowAlert
   :WorkflowFieldUpdate
   :WorkflowFlowAction
   :WorkflowKnowledgePublish
   :WorkflowOutboundMessage
   :WorkflowRule
   :WorkflowSend
   :WorkflowTask])
