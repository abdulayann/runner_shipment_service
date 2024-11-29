package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class V1TenantSettingsResponse implements Serializable {
    private static final long serialVersionUID = 1L;
    private List<String> RestrictedItemsForCreditLimit;
    private Boolean EnableCreditLimitManagement;
    private Boolean IsCreditLimitWithFusionEnabled;
    private int CreditLimitOn;
    private Boolean IsGlobalFusionIntegrationEnabled;
    private String BusinessUnitName;
    private boolean EnableIGMDetails;
    private boolean GSTTaxAutoCalculation;
    private Boolean BillingServiceV2Enabled;
    private Boolean ShipmentServiceV2Enabled;
    private Boolean UseV2ScreenForBillCharges;
    private Boolean P100Branch;
    private String DPWDateFormat;
    private Integer VolumeDecimalPlace;
    private Integer WeightDecimalPlace;
    private Integer WVDigitGrouping;
    private Integer WVGroupingNumber;
    private Boolean RoundoffLocalCurrencyAmount;
    private Boolean IsGroupingOverseas;
    private Integer CurrencyDigitGrouping;
    private Integer CurrencyGroupingNumber;
    private Integer DecimalValueForVolumetricWeight;
    private Boolean EnableAirMessaging;
    private Integer CurrencyDecimalPlace;
    private Boolean EnableEstimateAndActualDateTimeUpdates;
    private Boolean LogicAppIntegrationEnabled;
    private Boolean TransportOrchestratorEnabled;
    private Boolean IsMAWBColoadingEnabled;
    private Boolean IsColoadingMAWBStationEnabled;
    private List<Integer> ColoadingBranchIds;
    private Boolean FetchRatesMandate;
    private Boolean CarrierChangeAllowed;
    private Boolean isModuleValidationEnabled;
    @JsonProperty("EnableConsolSplitBillCharge")
    private Boolean enableConsolSplitBillCharge;
    @JsonProperty("ConsolidationAttachDefaultToMailId")
    private String consolidationAttachDefaultToMailId;
    @JsonProperty("ConsolidationAttachDefaultCCMailId")
    private String consolidationAttachDefaultCCMailId;
    @JsonProperty("ShipmentAttachDefaultToMailId")
    private String shipmentAttachDefaultToMailId;
    @JsonProperty("ShipmentAttachDefaultCCMailId")
    private String shipmentAttachDefaultCCMailId;
    @JsonProperty("LegalEntityCode")
    private String legalEntityCode;
    @JsonProperty("TransportModeConfig")
    private Boolean transportModeConfig;
    @JsonProperty("BookingTransportModeAir")
    private Boolean bookingTransportModeAir;
    @JsonProperty("BookingTransportModeSea")
    private Boolean bookingTransportModeSea;
    @JsonProperty("BookingTransportModeRail")
    private Boolean bookingTransportModeRail;
    @JsonProperty("BookingTransportModeRoad")
    private Boolean bookingTransportModeRoad;
    @JsonProperty("ShipmentTransportModeAir")
    private Boolean shipmentTransportModeAir;
    @JsonProperty("ShipmentTransportModeSea")
    private Boolean shipmentTransportModeSea;
    @JsonProperty("ShipmentTransportModeRail")
    private Boolean shipmentTransportModeRail;
    @JsonProperty("ShipmentTransportModeRoad")
    private Boolean shipmentTransportModeRoad;
    @JsonProperty("FileTransferConfigurations")
    private List<FileTransferConfigurations> fileTransferConfigurations;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class FileTransferConfigurations implements Serializable {
        @JsonProperty("Id")
        private Long id;
        @JsonProperty("Guid")
        private String guid;
        @JsonProperty("TransportMode")
        private String transportMode;
        @JsonProperty("CriteriaField")
        private int criteriaField;
        @JsonProperty("TriggerType")
        private int triggerType;
        @JsonProperty("IntervalTime")
        private int intervalTime;
        @JsonProperty("IntervalTimeUnit")
        private int intervalTimeUnit;
        @JsonProperty("IsActive")
        private int isActive;
    }
    
}
