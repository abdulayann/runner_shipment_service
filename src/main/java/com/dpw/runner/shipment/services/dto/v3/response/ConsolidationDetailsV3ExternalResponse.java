package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.Map;
import java.time.LocalDateTime;
import java.util.UUID;
import java.util.List;


@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Data
public class ConsolidationDetailsV3ExternalResponse extends ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private String deliveryMode;
    private LocalDateTime latestFullEquDeliveredToCarrier;
    private LocalDateTime earliestEmptyEquPickUp;
    private LocalDateTime earliestDropOffFullEquToCarrier;
    private ShipmentWtVolResponse shipmentWtVolResponse;
    private String sendingAgentCountry;
    private String receivingAgentCountry;
    private PartiesExternalResponse sendingAgent;
    private PartiesExternalResponse borrowedFrom;
    private PartiesExternalResponse receivingAgent;
    private List<String> shipmentIds;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<PartiesExternalResponse> consolidationAddresses;
    private List<String> houseBills;
    private Map<String, String> masterData;
    private Map<String, String> currenciesMasterData;
    private String carrierBookingRef;
    private Map<String, String> unlocationData;
    private Map<String, String> textData;
    private String modeOfBooking;
    private Map<String, String> tenantIdsData;
    private Boolean reefer;
    private String efreightStatus;
    private AwbStatus linkedHawbStatus;
    private UUID sourceGuid;
    private AwbStatus awbStatus;
    private String emergencyContactNumberCode;
    private Boolean creatingFromDgShipment;
    private String emergencyContactNumber;
    private List<String> screeningStatus;
    private String aomFreeText;
    private String securityStatus;
    private String exemptionCodes;
    private PartiesExternalResponse consigner;
    private PartiesExternalResponse consignee;
    private String additionalSecurityInformation;
    private PartiesExternalResponse client;
    private String sci;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    private String transferStatus;
    private String coLoadCarrierName;
    private String partner;
    private Long shipmentsCount;
    private Long bookingAgent;
    private Long originBranch;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Boolean borrowed;
    private String incoterms;
    private Boolean triggerMigrationWarning;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private MigrationStatus migrationStatus;
    private List<RoutingsResponse> routingsList;
    private List<PackingResponse> packingList;
    private List<EventsResponse> eventsList;
    private List<ContainerResponse> containersList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
}
