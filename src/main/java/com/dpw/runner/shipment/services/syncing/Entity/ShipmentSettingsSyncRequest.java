package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class ShipmentSettingsSyncRequest implements IRunnerRequest {

    @JsonProperty("RestrictAWBEdit")
    private Boolean RestrictAWBEdit;
    @JsonProperty("RestrictBLEdit")
    private Boolean RestrictBLEdit;
    @JsonProperty("AutoUpdateShipmentAWB")
    private Boolean AutoUpdateShipmentAWB;
    @JsonProperty("AutoUpdateShipmentBL")
    private Boolean AutoUpdateShipmentBL;
    @JsonProperty("TenantId")
    private Integer TenantId;
    @JsonProperty("HousebillNumberLock")
    private Boolean HousebillNumberLock;
    @JsonProperty("RestrictHblGen")
    private Boolean RestrictHblGen;
    @JsonProperty("PrintPhoneNumber")
    private Boolean PrintPhoneNumber;
    @JsonProperty("HousebillPrefix")
    private String HousebillPrefix;
    @JsonProperty("HousebillNumberGeneration")
    private String HousebillNumberGeneration;
    @JsonProperty("FooterColumns")
    private Integer FooterColumns;
    @JsonProperty("IsAutoPopulateShipType")
    private Boolean IsAutoPopulateShipType;
    @JsonProperty("PartialCloningEnabled")
    private Boolean PartialCloningEnabled;
    @JsonProperty("ShipConsolidationContainerEnabled")
    private Boolean ShipConsolidationContainerEnabled;
    @JsonProperty("AutoAttachConsolidation")
    private Boolean AutoAttachConsolidation;
    @JsonProperty("MultipleShipmentEnabled")
    private Boolean MultipleShipmentEnabled;
    @JsonProperty("EnableRouteMaster")
    private Boolean EnableRouteMaster;
    @JsonProperty("ArApFlag")
    private Boolean ArApFlag;
    @JsonProperty("ShipmentTiCargesLinkage")
    private Boolean ShipmentTiCargesLinkage;
    @JsonProperty("CooldownTime")
    private Integer CooldownTime;
    @JsonProperty("AdvancePeriod")
    private Integer AdvancePeriod;
    @JsonProperty("AutoEventCreate")
    private Boolean AutoEventCreate;
    @JsonProperty("ShipmentLite")
    private Boolean ShipmentLite;
    @JsonProperty("BillingLite")
    private Boolean BillingLite;
    @JsonProperty("RestrictedLocationsEnabled")
    private Boolean RestrictedLocationsEnabled;
    @JsonProperty("IsAtdAtaAutoPopulateEnabled")
    private Boolean IsAtdAtaAutoPopulateEnabled;
    @JsonProperty("RestrictedLocations")
    private List<String> RestrictedLocations;
    @JsonProperty("ShipmentImportApproverRole")
    private String ShipmentImportApproverRole;
    @JsonProperty("IsLowMarginApprovalRequired")
    private Boolean IsLowMarginApprovalRequired;
    @JsonProperty("HblTermsConditionTemplateRow")
    private List<HblTermsConditionTemplateDto> HblTermsConditionTemplateRow;
    @JsonProperty("HblHawbBackPrintTemplateRow")
    private List<HblTermsConditionTemplateDto> HblHawbBackPrintTemplateRow;
    @JsonProperty("InlineEnabled")
    private Boolean InlineEnabled;
    @JsonProperty("CarrierMandatory")
    private Boolean CarrierMandatory;
    @JsonProperty("WeightVolumeDecimalPlace")
    private Integer WeightVolumeDecimalPlace;
    @JsonProperty("DpwDateFormat")
    private String DpwDateFormat;
    @JsonProperty("WeightChargeableUnit")
    private String WeightChargeableUnit;
    @JsonProperty("VolumeChargeableUnit")
    private String VolumeChargeableUnit;
    @JsonProperty("MeasurementChargeableUnit")
    private String MeasurementChargeableUnit;
    @JsonProperty("TemperatureUnit")
    private String TemperatureUnit;
    @JsonProperty("DefaultTransportMode")
    private String DefaultTransportMode;
    @JsonProperty("DefaultContainerType")
    private String DefaultContainerType;
    @JsonProperty("DefaultShipmentType")
    private String DefaultShipmentType;
    @JsonProperty("EmailHost")
    private String EmailHost;
    @JsonProperty("EmailPort")
    private Long EmailPort;
    @JsonProperty("EmailUseSsl")
    private Boolean EmailUseSsl;
    @JsonProperty("EmailFrom")
    private String EmailFrom;
    @JsonProperty("EmailUserName")
    private String EmailUserName;
    @JsonProperty("EmailPassword")
    private String EmailPassword;
    @JsonProperty("EmailIsfAsmResponse")
    private Boolean EmailIsfAsmResponse;
    @JsonProperty("DefaultFailureEmailIds")
    private String DefaultFailureEmailIds;
    @JsonProperty("ShipmentLock")
    private Boolean ShipmentLock;
    @JsonProperty("OrgPrefix")
    private String OrgPrefix;
    @JsonProperty("OrgCodeLock")
    private Boolean OrgCodeLock;
    @JsonProperty("CountryPrefix")
    private Boolean CountryPrefix;
    @JsonProperty("OrgNumberGeneration")
    private String OrgNumberGeneration;
    @JsonProperty("UnLocoCodeEnabled")
    private Boolean UnLocoCodeEnabled;
    @JsonProperty("EnableOrgAddressUpdate")
    private Boolean EnableOrgAddressUpdate;
    @JsonProperty("AllowFreeTextAddress")
    private Boolean AllowFreeTextAddress;
    @JsonProperty("IsConsolidator")
    private Boolean IsConsolidator;
    @JsonProperty("MergeContainers")
    private Boolean MergeContainers;
    @JsonProperty("BolNumberLock")
    private Boolean BolNumberLock;
    @JsonProperty("IsShowMBL")
    private Boolean IsShowMBL;
    @JsonProperty("ConsolidationLite")
    private Boolean ConsolidationLite;
    @JsonProperty("ContainerEventBulkUpload")
    private Boolean ContainerEventBulkUpload;
    @JsonProperty("BolNumberPrefix")
    private String BolNumberPrefix;
    @JsonProperty("BolNumberGeneration")
    private String BolNumberGeneration;
    @JsonProperty("IsMandateDocType")
    private Boolean IsMandateDocType;
    @JsonProperty("HouseMainPage")
    private String HouseMainPage;
    @JsonProperty("HblFooter")
    private String HblFooter;
    @JsonProperty("PrintAfterEachPage")
    private Boolean PrintAfterEachPage;
    @JsonProperty("SeawayMainPage")
    private String SeawayMainPage;
    @JsonProperty("ShipTruckWayBillMainPage")
    private String ShipTruckWayBillMainPage;
    @JsonProperty("ConsTruckWayBillMainPage")
    private String ConsTruckWayBillMainPage;
    @JsonProperty("ShipTruckDriverProof")
    private String ShipTruckDriverProof;
    @JsonProperty("ConsTruckDriverProof")
    private String ConsTruckDriverProof;
    @JsonProperty("CommercialInvMainPage")
    private String CommercialInvMainPage;
    @JsonProperty("PackingListMainPage")
    private String PackingListMainPage;
    @JsonProperty("CustomsInsMainPage")
    private String CustomsInsMainPage;
    @JsonProperty("AirwayMainPage")
    private String AirwayMainPage;
    @JsonProperty("CanMainPage")
    private String CanMainPage;
    @JsonProperty("CanBackPrint")
    private String CanBackPrint;
    @JsonProperty("ArrivalNotice")
    private String ArrivalNotice;
    @JsonProperty("FreightCertification")
    private String FreightCertification;
    @JsonProperty("PreAlertDoc")
    private String PreAlertDoc;
    @JsonProperty("ManifestPrint")
    private String ManifestPrint;
    @JsonProperty("ContainerManifestPrint")
    private String ContainerManifestPrint;
    @JsonProperty("SeaImportShipmentManifest")
    private String SeaImportShipmentManifest;
    @JsonProperty("SeaExportShipmentManifest")
    private String SeaExportShipmentManifest;
    @JsonProperty("SeaImportConsolManifest")
    private String SeaImportConsolManifest;
    @JsonProperty("SeaExportConsolManifest")
    private String SeaExportConsolManifest;
    @JsonProperty("AirImportShipmentManifest")
    private String AirImportShipmentManifest;
    @JsonProperty("AirExportShipmentManifest")
    private String AirExportShipmentManifest;
    @JsonProperty("AirImportConsolManifest")
    private String AirImportConsolManifest;
    @JsonProperty("AirExportConsolManifest")
    private String AirExportConsolManifest;
    @JsonProperty("ProofOfDelivery")
    private String ProofOfDelivery;
    @JsonProperty("PickupOrder")
    private String PickupOrder;
    @JsonProperty("DeliveryOrder")
    private String DeliveryOrder;
    @JsonProperty("BookingConfirmation")
    private String BookingConfirmation;
    @JsonProperty("CostalDocument")
    private String CostalDocument;
    @JsonProperty("ShipmentInstruction")
    private String ShipmentInstruction;
    @JsonProperty("ConsolidatedPackingList")
    private String ConsolidatedPackingList;
    @JsonProperty("AwbLable")
    private String AwbLable;
    @JsonProperty("CargoManifest")
    private String CargoManifest;
    @JsonProperty("PackingListMainPageAir")
    private String PackingListMainPageAir;
    @JsonProperty("FreightCertificationAir")
    private String FreightCertificationAir;
    @JsonProperty("PreAlertAir")
    private String PreAlertAir;
    @JsonProperty("BookingConfirmationAir")
    private String BookingConfirmationAir;
    @JsonProperty("PickupOrderAir")
    private String PickupOrderAir;
    @JsonProperty("DeliveryOrderAir")
    private String DeliveryOrderAir;
    @JsonProperty("CanMainPageAir")
    private String CanMainPageAir;
    @JsonProperty("CanBackPrintAir")
    private String CanBackPrintAir;
    @JsonProperty("CustomsInsMainPageAir")
    private String CustomsInsMainPageAir;
    @JsonProperty("CommercialInvMainPageAir")
    private String CommercialInvMainPageAir;
    @JsonProperty("ArrivalNoticeAir")
    private String ArrivalNoticeAir;
    @JsonProperty("Hawb")
    private String Hawb;
    @JsonProperty("Mawb")
    private String Mawb;
    @JsonProperty("AwbNeutral")
    private String AwbNeutral;
    @JsonProperty("CSR")
    private String CSR;
    @JsonProperty("ShippingRequestMainPage")
    private String ShippingRequestMainPage;
    @JsonProperty("ShippingRequestAir")
    private String ShippingRequestAir;
    @JsonProperty("SeaShippingInstructionMainPage")
    private String SeaShippingInstructionMainPage;
    @JsonProperty("DefaultEmailTemplate")
    private String DefaultEmailTemplate;
    @JsonProperty("DecimalPlaces")
    private Integer DecimalPlaces;
    @JsonProperty("HblApprovalRole")
    private Integer HblApprovalRole;
    @JsonProperty("CargoFinanceBooking")
    private Boolean CargoFinanceBooking;
    @JsonProperty("UnicoBlObject")
    private Boolean UnicoBlObject;
    @JsonProperty("HblLock")
    private List<HblLockDto> HblLock;
    @JsonProperty("CustomisedSequence")
    private Boolean CustomisedSequence;
    @JsonProperty("TenantProducts")
    private List<TenantProductsDto> TenantProducts;
    @JsonProperty("ProductSequenceConfig")
    private List<ProductSequenceConfigDto> ProductSequenceConfig;
    @JsonProperty("HawbLock")
    private List<HawbLockDto> HawbLock;
    @JsonProperty("MawbLock")
    private List<MawbLockDto> MawbLock;
    @JsonProperty("HblMultipleOriginalApproval")
    private Integer hblMultipleOriginalApproval;
    @JsonProperty("HblApprovalFlow")
    private Boolean hblApprovalFlow;
    @JsonProperty("IsShipmentLevelContainer")
    private Boolean isShipmentLevelContainer;
    @JsonProperty("RestrictBlRelease")
    private Boolean restrictBlRelease;
    @JsonProperty("RestrictBlApprovalRole")
    private Integer restrictBlApprovalRole;
    @JsonProperty("EManifest")
    private Boolean eManifest;
    @JsonProperty("ISFFileMainPage")
    private String ISFFileMainPage;
    @JsonProperty("WeightDecimalPlace")
    private Integer weightDecimalPlace;
    @JsonProperty("VolumeDecimalPlace")
    private Integer volumeDecimalPlace;
    @JsonProperty("CancelledBLSuffix")
    private String cancelledBLSuffix;
    @JsonProperty("RegulatedAgent")
    private Boolean regulatedAgent;
    @JsonProperty("RANumber")
    private String raNumber;
    @JsonProperty("RAExpiry")
    private LocalDateTime raExpiry;
    @JsonProperty("TransportOrderRoad")
    private String transportOrderRoad;
    @JsonProperty("DisableBlPartiesName")
    private Boolean disableBlPartiesName;
    @JsonProperty("AirDGFlag")
    private Boolean airDGFlag;
    @JsonProperty("IataTactFlag")
    private Boolean iataTactFlag;
    @JsonProperty("EnableLCLConsolidation")
    private Boolean enableLclConsolidation;
    @JsonProperty("BookingOrder")
    private String BookingOrder;
    @JsonProperty("BookingOrderForMbl")
    private String BookingOrderForMbl;
    @JsonProperty("BookingOrderAir")
    private String BookingOrderAir;
    @JsonProperty("BookingOrderAirforMawb")
    private String BookingOrderAirForMawb;
    @JsonProperty("TransportInstructionPickupOrder")
    private String transportInstructionPickupOrder;
    @JsonProperty("TransportInstructionDeliveryOrder")
    private String transportInstructionDeliveryOrder;
    @JsonProperty("EnablePartyCheckForConsolidation")
    private Boolean enablePartyCheckForConsolidation;
    @JsonProperty("CSD")
    private String csd;
    @JsonProperty("HideManifest")
    private Boolean hideManifest;
    @JsonProperty("FcrDocument")
    private String fcrDocument;
    @JsonProperty("CountryAirCargoSecurity")
    private Boolean countryAirCargoSecurity;
    @JsonProperty("AmrAirFreight")
    private Boolean isAmrAirFreightEnabled;
}
