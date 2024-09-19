package com.dpw.runner.shipment.services.dto.TO.fzb;

import com.dpw.runner.shipment.services.entity.TransportLogisticsPackage;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class HouseConsignmentItemFZB {

    @JsonProperty("SequenceNumber")
    @NotNull(message = "Line-item number is mandatory")
    //@Pattern(regexp = "\\d{1,3}", message = "Line-item number must be a 1 to 3-digit number")
    private Integer sequenceNumeric;

    @JsonProperty("TypeCode")
    //@NotNull(message = "Type code is mandatory")
    @Pattern(regexp = "\\d{6,18}", message = "Type code must be a 6 to 18-digit number")
    private String typeCode;

    @JsonProperty("GrossWeight")
    @NotNull(message = "Gross weight measure is mandatory")
    private WeightMeasureFZB grossWeightMeasure;

    private WeightMeasureFZB grossVolumeMeasure;

    private ChargeAmountFZB totalChargeAmount;

    //@Min(value = 1, message = "Package quantity must be at least 1")
    private Integer packageQuantity;

    @JsonProperty("PieceQuantity")
    @NotNull(message = "Piece quantity is mandatory")
    @Min(value = 1, message = "Piece quantity must be at least 1")
    private Integer pieceQuantity;

    private String volumetricFactor;

    private String information;

    @NotNull(message = "Nature identification is mandatory")
    @StringModifier(maxLength = 20, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 20, message = "Nature identification must not exceed 20 characters")
    private String natureIdentificationTransportCargo;

    @JsonProperty("OriginCountryId")
    @NotNull(message = "Origin country ID is mandatory")
    @Pattern(regexp = "[A-Z]{2}", message = "Origin country ID must be a 2-letter uppercase code")
    private String originCountryId;

    @Valid
    private List<AssociatedUnitLoadTransportEquipmentFZB> associatedUnitLoadTransportEquipmentList;

    @Valid
    private List<TransportLogisticsPackage> transportLogisticsPackageList;

    @JsonProperty("AppFreightRateServiceCharge")
    @Valid
    private FreightRateServiceChargeFZB applicableFreightRateServiceCharge;

    @Size(min = 3, max = 5, message = "ID must be 3 to 5 characters long")
    private String specifiedRateCombinationPointLocationId;

}
