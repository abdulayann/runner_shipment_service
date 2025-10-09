package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Generated
@ToString(onlyExplicitlyIncluded = true)
public class ShipmentRetrieveExternalResponse extends BaseShipmentResponse {
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadyDate;
    private PartiesExternalResponse consignee;
    private PartiesExternalResponse consigner;
    private PartiesExternalResponse client;
    private AdditionalDetailExternalResponse additionalDetails;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<PartiesExternalResponse> shipmentAddresses;
    private String fmcTlcId;
    private Map<String, Long> containerData;
    private Map<String, String> textData;
    private UUID sourceGuid;
    private List<String> implicationList;
    private OceanDGStatus oceanDGStatus;
    private Long bookingId;
    private Boolean isPacksAvailable = Boolean.FALSE;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtOriginDate;
    private Long consolidationId;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private Integer dgPacksCount;
    private Boolean isEmptyWeightPackAvailable = Boolean.FALSE;
    private Boolean isInterBranchConsoleAttached;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Boolean isVolumeEditable = Boolean.FALSE;
    private List<RoutingsLiteResponse> routingsLiteResponses;
    private String consolBookingNumber;
    private String dgPacksUnit;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private Boolean isCargoSummaryEditable = Boolean.FALSE;
    private List<RoutingsResponse> routingsList;
    private List<PackingResponse> packingList;
    private List<EventsResponse> eventsList;
    private Set<ContainerResponse> containersList;
    private List<NotesResponse> customerBookingNotesList;
    private List<Notes> notesList;
    private Set<ConsolidationListIdExternalResponse> consolidationList;
}