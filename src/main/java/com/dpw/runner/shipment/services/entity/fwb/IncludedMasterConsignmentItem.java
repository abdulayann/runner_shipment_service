package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.entity.TransportLogisticsPackage;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IncludedMasterConsignmentItem {

    @JsonProperty("SequenceNumeric")
    @Min(value = 0, message = "Included master consignment item sequence numeric must be greater than or equal to 0")
    @Max(value = 99, message = "Included master consignment item sequence numeric must be less than or equal to 99")
    @NotNull(message = "Included master consignment item sequence numeric cannot be null")
    private Integer sequenceNumeric;

    @JsonProperty("TypeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid included master consignment item type code provided")
    @Size(min = 6,max = 18, message = "Included master consignment item type code should have length ({min}, {max})")
    //@NotNull(message = "Included master consignment item type code cannot be null")
    private String typeCode;

    //todo: if typeCode has value then typeCodeListAgencyId cannot be null
    @JsonProperty("TypeCodeListAgencyId")
    private String typeCodeListAgencyId = "1";

    @JsonProperty("GrossWeightMeasure")
    @DecimalMin(value = "0.1", message = "Included master consignment item gross weight measure should be >= 0.1")
    @DecimalMax(value = "9999999", message = "Included master consignment item gross weight measure should be <= 9999999")
    private Double grossWeightMeasure;

    @JsonProperty("GrossWeightMeasureUnit")
    private String grossWeightMeasureUnit;

    @JsonProperty("GrossVolumeMeasure")
    @DecimalMin(value = "0.01", message = "Included master consignment item gross volume measure should be >= 0.01")
    @DecimalMax(value = "999999999", message = "Included master consignment item gross volume measure should be <= 999999999")
    private Double grossVolumeMeasure;

    @JsonProperty("GrossVolumeMeasureUnit")
    private String grossVolumeMeasureUnit;

    // TODO: Note: Not for ULDs as that is included in the ULD Line below
    @JsonProperty("PackageQuantity")
    @Min(value = 0, message = "Included master consignment item package quantity must be greater than or equal to 0")
    @Max(value = 99999, message = "Included master consignment item package quantity must be less than or equal to 99999")
    private Integer packageQuantity;

    @JsonProperty("PieceQuantity")
    @Min(value = 0, message = "Included master consignment item piece quantity must be greater than or equal to 0")
    @Max(value = 9999, message = "Included master consignment item piece quantity must be less than or equal to 9999")
    private Integer pieceQuantity;

    // TODO: If the Volume amount is specified, this element can be left blank
    @JsonProperty("VolumetricFactor")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid included master consignment item volumetric factor provided")
    @Size(max = 8, message = "Included master consignment item volumetric factor can  have max length {max}")
    private String volumetricFactor;

    // TODO: No Dimensions Available (NDA) must be NDA if ULD and Volume Details are not included and no meaningful dimensions are available.
    //  This condition is applicable on the rating segment level rather than each rating line.
    @JsonProperty("Information")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included master consignment item information provided")
    @Size(max = 3, message = "Included master consignment item information can have max length {max}")
    private String information;

    @JsonProperty("NatureIdentificationTransportCargoIdentification")
    @Size(max = 70, message = "Nature identification transport cargo identification can have max length {max}")
    private String natureIdentificationTransportCargoIdentification;

    @JsonProperty("OriginCountryId")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included master consignment item origin country id provided")
    @Size(max = 2, message = "Included master consignment item origin country id can have max length {max}")
    private String originCountryId;

    @Valid
    @JsonProperty("AssociatedUnitLoadTransportEquipment")
    private AssociatedUnitLoadTransportEquipment associatedUnitLoadTransportEquipment;

    @Valid
    @JsonProperty("TransportLogisticsPackage")
    private List<TransportLogisticsPackage> transportLogisticsPackageList;

    @Valid
    @JsonProperty("ApplicableFreightRateServiceCharge")
    private ApplicableFreightRateServiceCharge applicableFreightRateServiceCharge;

    @JsonProperty("SpecifiedRateCombinationPointLocationId")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid specified rate combination point location id provided")
    @Size(max = 5, message = "Specified rate combination point location id can have max length {max}")
    private String specifiedRateCombinationPointLocationId;

    @Valid
    @JsonProperty("ApplicableUnitLoadDeviceRateClass")
    private ApplicableUnitLoadDeviceRateClass applicableUnitLoadDeviceRateClass;
}
