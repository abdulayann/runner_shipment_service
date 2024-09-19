package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class AssociatedStatusConsignment {

    @JacksonXmlProperty(localName ="GrossWeightMeasure")
    private GrossWeightMeasure grossWeightMeasure;

    @JacksonXmlProperty(localName ="GrossVolumeMeasure")
    private TotalGrossWeightMeasure grossVolumeMeasure;

    @JacksonXmlProperty(localName ="DensityGroupCode")
    @DecimalMin(value = "0.0", message = "Status consignment density group code must be greater than or equal to 0.0")
    @DecimalMax(value = "99", message = "Status consignment density group code must be less than or equal to 99")
    private Integer densityGroupCode;

    @JacksonXmlProperty(localName ="PieceQuantity")
    @NotNull(message = "Status Consignment Piece quantity cannot be null")
    private Integer pieceQuantity;

    @JacksonXmlProperty(localName ="TransportSplitDescription")
    @NotNull(message = "Transport split description cannot be null")
    @Size(max = 1, message = "Transport split description can have max length {max}")
    private String transportSplitDescription;

    @JacksonXmlProperty(localName ="DiscrepancyDescriptionCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Discrepancy description code provided")
    @Size(max = 4, message = "Discrepancy description code can have max length {max}")
    private String discrepancyDescriptionCode;

    @JacksonXmlProperty(localName ="AssociatedManifestDocument")
    private AssociatedManifestDocument associatedManifestDocument;

    @JacksonXmlProperty(localName ="ApplicableLogisticsServiceCharge")
    private ApplicableLogisticsServiceCharge applicableLogisticsServiceCharge;

    @Valid
    @JacksonXmlProperty(localName ="SpecifiedLogisticsTransportMovement")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSULogisticsTransportMovement> specifiedLogisticsTransportMovement;

    @JacksonXmlProperty(localName ="NotifiedParty")
    private NotifyParty notifyParty;

    @JacksonXmlProperty(localName ="DeliveryParty")
    private DeliveryParty deliveryParty;

    @Valid
    @JacksonXmlProperty(localName ="AssociatedReceivedFromParty")
    private AssociatedReceivedFromParty associatedReceivedFromParty;

    @Valid
    @JacksonXmlProperty(localName ="AssociatedTransferredFromParty")
    private AssociatedTransferredFromParty associatedTransferredFromParty;

    @Valid
    @JacksonXmlProperty(localName ="HandlingOSIInstructions")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSUHandlingOSIInstructions> handlingOSIInstructions;

    @Valid
    @JacksonXmlProperty(localName ="IncludedCustomsNote")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSUIncludedCustomsNote> includedCustomsNote;

    @JacksonXmlProperty(localName ="AssociatedConsignmentCustomsProcedureCode")
    private AssociatedConsignmentCustomsProcedure associatedConsignmentCustomsProcedure;

    @Valid
    @JacksonXmlProperty(localName ="UtilizedUnitLoadTransportEquipment")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSUUtilizedUnitLoadTransportEquipment> utilizedUnitLoadTransportEquipment;

    @Valid
    @JacksonXmlProperty(localName ="IncludedHouseConsignment")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSUIncludedHouseConsignment> includedHouseConsignment;



}
