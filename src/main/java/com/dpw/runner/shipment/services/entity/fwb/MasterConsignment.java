package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.entity.IncludedAccountingNote;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MasterConsignment {

    // shipment ref number
    @JsonProperty("ID")
    @Size(max = 14, message = "Master Consignment id should be of max length {max}")
    @Pattern(regexp = "^[a-zA-Z0-9.\\- ]*$", message = "Master Consignment id FWB Pattern mismatch")
    private String id;

    @JsonProperty("AdditionalID")
    @Size(max = 12, message = "Master Consignment additional id should be of max length {max}")
    private String additionalID;

    @JsonProperty("FreightForwarderAssignedID")
    @Size(max = 15, message = "Master Consignment freight forwarder assigned id should be of max length {max}")
    private String freightForwarderAssignedID;

    @JsonProperty("AssociatedReferenceID")
    @Size(max = 15, message = "Master Consignment associated reference id should be of max length {max}")
    private String associatedReferenceID;

    @JsonProperty("NilCarriageValueIndicator")
    private Boolean nilCarriageValueIndicator;

//    @JsonProperty("NilCarriageValueIndicatorSpecified")
//    private Boolean nilCarriageValueIndicatorSpecified;

    @JsonProperty("DeclaredValueForCarriageAmount")
    @DecimalMin(value = "0.0", message = "Declared Value For Carriage Amount must be greater than or equal to 0.0")
    @DecimalMax(value = "999999999", message = "Declared Value For Carriage Amount must be greater than or equal to 999999999")
    private Double declaredValueForCarriageAmount;

    @JsonProperty("DeclaredValueForCarriageCurrency")
    // TODO: if declaredValueForCarriageAmount has value the declaredValueForCarriageCurrency cannot e null
    private String declaredValueForCarriageCurrency;

    @JsonProperty("NilCustomsValueIndicator")
    private Boolean nilCustomsValueIndicator;

//    @JsonProperty("NilCustomsValueIndicatorSpecified")
//    private Boolean nilCustomsValueIndicatorSpecified;

    @JsonProperty("DeclaredValueForCustomsAmount")
    @DecimalMin(value = "0.0", message = "Declared value for customs amount must be greater than or equal to 0.0")
    @DecimalMax(value = "999999999", message = "Declared value for customs amount must be greater than or equal to 999999999")
    private Double declaredValueForCustomsAmount;

    @JsonProperty("DeclaredValueForCustomsCurrency")
    // TODO: if declaredValueForCustomsAmount has value the declaredValueForCustomsCurrency cannot e null
    private String declaredValueForCustomsCurrency;

    @JsonProperty("NilInsuranceValueIndicator")
    private Boolean nilInsuranceValueIndicator;

//    @JsonProperty("NilInsuranceValueIndicatorSpecified")
//    private Boolean nilInsuranceValueIndicatorSpecified;

    @JsonProperty("InsuranceValueAmount")
    @DecimalMin(value = "0.0", message = "Insurance value amount must be greater than or equal to 0.0")
    @DecimalMax(value = "99999999", message = "Insurance value amount must be greater than or equal to 99999999")
    private Double insuranceValueAmount;

    @JsonProperty("InsuranceValueCurrency")
    // TODO: if insuranceValueAmount has value the insuranceValueCurrency cannot e null
    private Double insuranceValueCurrency;

    @JsonProperty("TotalChargePrepaidIndicator")
    @NotNull(message = "Total Charge Prepaid Indicator is required")
    private Boolean totalChargePrepaidIndicator;

    @JsonProperty("TotalDisbursementPrepaidIndicator")
    @NotNull(message = "Total Disbursement Prepaid Indicator is required")
    private Boolean totalDisbursementPrepaidIndicator;

    @JsonProperty("IncludedTareGrossWeightMeasureWeight")
    @DecimalMin(value = "0.0", message = "Included tare gross weight must be greater than or equal to 0.0")
    @DecimalMax(value = "9999999", message = "Included tare gross weight must be greater than or equal to 9999999")
    @NotNull(message = "TareGrossWeight is required")
    private Double includedTareGrossWeightMeasureWeight;

    @JsonProperty("IncludedTareGrossWeightMeasureUnitCode")
    @NotNull(message = "TareGrossWeight Unit is required")
    // TODO: if includedTareGrossWeightMeasureWeight has value the includedTareGrossWeightMeasureUnitCode cannot e null
    private String includedTareGrossWeightMeasureUnitCode;

    @Valid
    @JsonProperty("GrossVolumeMeasureWeight")
    private Measure grossVolumeMeasureWeight;

    @JsonProperty("GrossVolumeMeasureUnitCode")
    // TODO: if grossVolumeMeasureWeight has value the grossVolumeMeasureUnitCode cannot e null
    private String grossVolumeMeasureUnitCode;

    @JsonProperty("DensityGroupCode")
    @Min(value = 0, message = "Density group code value must be greater than or equal to 0")
    @Max(value = 99, message = "Density group code value must be less than or equal to 99")
    private Integer densityGroupCode;

    @JsonProperty("PackageQuantity")
    @Min(value = 0, message = "Package quantity value must be greater than or equal to 0")
    @Max(value = 99999, message = "Package quantity value must be less than or equal to 99999")
    private Integer packageQuantity;

    @JsonProperty("TotalPieceQuantity")
    @NotNull(message = "Total piece quantity cannot be null")
    @Min(value = 0, message = "Total piece quantity must be greater than or equal to 0")
    @Max(value = 9999, message = "Total piece quantity must be less than or equal to 9999")
    private Integer totalPieceQuantity;

    @JsonProperty("ProductID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid product id provided")
    @Size(max = 70, message = "Product Id can be of max length {max} ")
    private String productID;

    @Valid
    @JsonProperty("ConsignorParty")
    @NotNull(message = "Consignor party cannot be null")
    private Party consignorParty;

    @Valid
    @JsonProperty("ConsigneeParty")
    @NotNull(message = "Consignor party cannot be null")
    private Party consigneeParty;

    @Valid
    @JsonProperty("FreightForwarderParty")
    private Party freightForwarderParty;

    @Valid
    @JsonProperty("AssociatedParty")
    private Party associatedParty;

    @Valid
    @JsonProperty("OriginLocation")
    @NotNull(message = "origin location cannot be null")
    private LocationDto originLocation;

    @Valid
    @JsonProperty("FinalDestinationLocation")
    @NotNull(message = "Final Destination Location cannot be null")
    private LocationDto finalDestinationLocation;

    @Valid
    @JsonProperty("SpecifiedLogisticsTransportMovement")
    private List<LogisticsTransportMovement> specifiedLogisticsTransportMovement;

    @Valid
    @JsonProperty("UtilizedLogisticsTransportEquipment")
    private List<TransportEquipment> utilizedLogisticsTransportEquipment;

    @Valid
    @JsonProperty("HandlingSPHInstructions")
    private List<HandlingSPHInstructions> handlingSPHInstructions;

    @Valid
    @JsonProperty("HandlingSSRInstructions")
    private List<HandlingSSRInstructions> handlingSSRInstructions;

    @Valid
    @JsonProperty("HandlingOSIInstructions")
    private List<HandlingOSIInstructions> handlingOSIInstructions;

    @Valid
    @JsonProperty("IncludedAccountingNote")
    private List<IncludedAccountingNote> includedAccountingNotes;

    @Valid
    @JsonProperty("IncludedCustomsNote")
    private List<IncludedCustomsNote> includedCustomsNote;

    @Valid
    @JsonProperty("AssociatedReferenceDocument")
    private List<AssociatedReferenceDocument> associatedReferenceDocument;

    // TODO: Code indicating the origin of goods for Customs purposes (e.g. For goods in free circulation in the EU)
    //TODO: Condition List to be provided by local customs authorities

    @JsonProperty("AssociatedConsignmentCustomsProcedureCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated consignment customs procedure code provided")
    @Size(max = 2, message = "Associated consignment customs procedure code can have max length {max}")
    private String associatedConsignmentCustomsProcedureCode;

    @JsonProperty("ApplicableOriginCurrencyExchangeCode")
    @StringModifier(maxLength = 3, pattern = StringModifier.PatternType.ALPHA)
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable origin currency exchange code provided")
    @Size(max = 3, message = "Applicable origin currency exchange code can have max length {max}")
    @NotNull(message = "Applicable origin currency exchange code cannot be null")
    private String applicableOriginCurrencyExchangeCode;

    @Valid
    @JsonProperty("ApplicableDestinationCurrencyExchange")
    private ApplicableDestinationCurrencyExchange applicableDestinationCurrencyExchange;

    @Valid
    @JsonProperty("ApplicableLogisticsServiceCharge")
    private ApplicableLogisticsServiceCharge applicableLogisticsServiceCharge;

    @Valid
    @JsonProperty("ApplicableLogisticsAllowanceCharge")
    private List<ApplicableLogisticsAllowanceCharge> applicableLogisticsAllowanceCharges;

    @Valid
    @JsonProperty("ApplicableRating")
    @Size(min = 1, message = "At least one applicable rating should be provided")
    private List<ApplicableRating> applicableRatings = new ArrayList<>();

    @Valid
    @JsonProperty("ApplicableTotalRating")
    @Size(min = 1, message = "At least one applicable total rating should be provided")
    private List<ApplicableTotalRating> applicableTotalRating;
}
