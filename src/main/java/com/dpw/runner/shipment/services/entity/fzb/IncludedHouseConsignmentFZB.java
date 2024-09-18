package com.dpw.runner.shipment.services.entity.fzb;

import com.dpw.runner.shipment.services.entity.IncludedAccountingNote;
import com.dpw.runner.shipment.services.entity.fwb.HandlingSPHInstructions;
import com.dpw.runner.shipment.services.entity.fwb.HandlingSSRInstructions;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class IncludedHouseConsignmentFZB {

    //@NotNull(message = "Shipment Reference Number is mandatory")
    @JsonProperty("ID")
    @Size(max = 14, message = "Shipment Reference Number must be less than or equal to 14 characters")
    @Pattern(regexp = "^[a-zA-Z0-9.\\- ]*$", message = "Shipment Reference Number FZB Pattern mismatch")
    private String id;

    //@NotNull(message = "Supplementary Shipment Information is mandatory")
    @Size(max = 12, message = "Supplementary Shipment Information must be less than or equal to 12 characters")
    private String additionId;

    //@NotNull(message = "Other party ref is mandatory")
    @Size(max = 15, message = "Other party ref must be less than or equal to 15 characters")
    private String associatedReferenceID;

    private Boolean nilCarriageValueIndicator;
    private Double declaredValueForCarriageAmount;
    private Boolean nilCustomsValueIndicator;
    private Double declaredValueForCustomsAmount;
    private Boolean nilInsuranceValueIndicator;
    private Double insuranceValueAmount;

    @JsonProperty("TotalChargePrepaidIndicator")
    @NotNull(message = "Prepaid/Collect Weight/Valuation Indicator is mandatory")
    //@Pattern(regexp = "[A-Za-z]", message = "Prepaid/Collect Weight/Valuation Indicator must be a single alphabetic character")
    private Boolean totalChargePrepaidIndicator;

    @JsonProperty("WeightChargeTotalAmount")
    @NotNull(message = "Weight Charge Amount is mandatory")
    //@Pattern(regexp = "\\p{Alnum}{1,12}", message = "Weight Charge Amount must be an alphanumeric string with at most 12 characters")
    private Double weightTotalChargeAmount;

    @JsonProperty("ValuationChargeTotalAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Valuation Charge Amount must be an alphanumeric string with at most 12 characters")
    private Double valuationTotalChargeAmount;

    @JsonProperty("TaxTotalAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Tax Amount must be an alphanumeric string with at most 12 characters")
    private Double taxTotalChargeAmount;

    @JsonProperty("TotalDisbursementPrepaidIndicator")
    @NotNull(message = "Prepaid/Collect Other Charges Indicator is mandatory")
    //@Pattern(regexp = "[A-Za-z]", message = "Prepaid/Collect Other Charges Indicator must be a single alphabetic character")
    private Boolean totalDisbursementPrepaidIndicator;

    @JsonProperty("AgentTotalDisbursementAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Total Other Charges Due Agent Amount must be an alphanumeric string with at most 12 characters")
    private Double agentTotalDisbursementAmount;

    @JsonProperty("CarrierTotalDisbursementAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Total Other Charges Due Carrier Amount must be an alphanumeric string with at most 12 characters")
    private Double carrierTotalDisbursementAmount;

    @JsonProperty("TotalPrepaidChargeAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Total Prepaid Charge Summary Amount must be an alphanumeric string with at most 12 characters")
    private Double totalPrepaidChargeAmount;

    @JsonProperty("TotalCollectChargeAmount")
    //@Pattern(regexp = "\\p{Alnum}{0,12}", message = "Total Collect Charge Summary Amount must be an alphanumeric string with at most 12 characters")
    private Double totalCollectChargeAmount;

    @JsonProperty("IncludedTareGrossWeightMeasure")
    @NotNull(message = "Total gross weight is mandatory")
    //@Pattern(regexp = "\\p{Alnum}{1,7}", message = "Total gross weight must be an alphanumeric string with at most 7 characters")
    private String includedTareGrossWeightMeasure;

    @JsonProperty("IncludedTareGrossWeightMeasureUnitCode")
    private String includedTareGrossWeightMeasureUnit;

    @Pattern(regexp = "\\d{0,9}(\\.\\d{1,9})?", message = "Total Volume must be a numeric string with up to 9 digits before and after the decimal point")
    private String grossVolumeMeasure;



    @Digits(integer = 5, fraction = 0, message = "Total Shipper's Load and Count must be a numeric value with at most 5 digits")
    private Integer packageQuantity;



    @JsonProperty("TotalPieceQuantity")
    @NotNull(message = "Total number of pieces is mandatory")
    @Digits(integer = 4, fraction = 0, message = "Total nuber of pieces must be a numeric value with at most 4 digits")
    private Integer totalPieceQuantity;

    @JsonProperty("SummaryDescription")
    @StringModifier(maxLength = 600, pattern = StringModifier.PatternType.TEXT)
    @NotNull(message = "Description of Goods is mandatory")
    @Size(max = 600, message = "Description of Goods must be at most 600 characters")
    private String summaryDescription;

    private String applicableTransportInsuranceParty;





    @JsonProperty("ConsignorParty")
    @NotNull
    @Valid
    private PartyFZB consignorParty;

    @JsonProperty("ConsigneeParty")
    @NotNull
    @Valid
    private PartyFZB consigneeParty;

//    @Valid
//    private PartyFZB freightForwarderParty;

//    @Valid
//    private PartyFZB associatedParty;

    @JsonProperty("OriginLocation")
    @NotNull
    @Valid
    private LocationFZB originLocation;

    @JsonProperty("FinalDestinationLocation")
    @NotNull
    @Valid
    private LocationFZB finalDestinationLocation;

    @JsonProperty("SpecifiedLogisticsTransportMovement")
    @Valid
    private List<LogisticsTransportMovementFZB> specifiedLogisticsTransportMovement;

    @JsonProperty("HandlingSPHInstructions")
    @Valid
    private List<HandlingSPHInstructions> handlingSPHInstructions;

    @JsonProperty("HandlingSSRInstructions")
    @Valid
    private List<HandlingSSRInstructions> handlingSSRInstructions;

//    @Valid
//    private List<HandlingOSIInstructions> handlingOSIInstructions;













//    @Pattern(regexp = "\\p{Alnum}{0,5}", message = "Total Number of Lines must be an alphanumeric string with at most 5 characters")
//    private Integer consignmentItemQualifier;

//    @Valid
//    private List<TransportEquipmentFZB> utilizedLogisticsTransportEquipment;

    @JsonProperty("IncludedAccountingNote")
    @Valid
    private List<IncludedAccountingNote> includedAccountingNotes;

    @JsonProperty("IncludedCustomsNote")
    @Valid
    private List<CustomsNoteFZB> includedCustomsNote;

//    @Valid
//    private CustomsProcedureFZB associatedConsignmentCustomsProcedure;

//    @Valid
//    @NotNull
//    private String applicableOriginCurrencyExchange;
//
//    @Valid
//    private CurrencyExchangeFZB applicableDestinationCurrencyExchange;

//    @Valid
//    private ApplicableLogisticsServiceChargeFZB applicableLogisticsServiceCharge;

    @Valid
    private List<ApplicableLogisticsAllowanceChargeFZB> applicableLogisticsAllowanceCharges;

    @JsonProperty("IncludedHouseConsignmentItem")
    @Valid
    private List<HouseConsignmentItemFZB> includedHouseConsignmentItem;

    @NotNull(message = "AmountCurrency is mandatory")
    @JsonProperty("AmountCurrency")
    private String amountCurrency;
}
