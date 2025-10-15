package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.BaseShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentDetailsQuoteDateType;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Generated
@SuppressWarnings("java:S1948")
public class ShipmentDetailsV3Response extends BaseShipmentResponse {
    private Boolean isDomestic;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    private String netWeightUnit;
    private Integer innerPacks;
    private String innerPackUnit;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadyDate;
    private Boolean autoUpdateWtVol;
    private Boolean containerAutoWeightVolumeUpdate;
    private String entryDetail;
    private Boolean isShipperClientEqual;
    private Boolean isConsigneeClientEqual;
    private Boolean cargoFinanceBooking = Boolean.FALSE;
    private String route;
    private Long documentationPartner;
    private Long triangulationPartner;
    private Boolean intraBranch = Boolean.FALSE;
    private Integer prevShipmentStatus;
    @JsonProperty("isShipmentReadOnly")
    private Boolean isShipmentReadOnly = Boolean.FALSE;
    private String financeClosedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime financeClosedOn;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private AdditionalDetailV3Response additionalDetails;
    private PickupDeliveryDetailsResponse pickupDetails;
    private PickupDeliveryDetailsResponse deliveryDetails;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private Long container20Count;
    private Long container40Count;
    private Long container20GPCount;
    private Long container20RECount;
    private Long container40GPCount;
    private Long container40RECount;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> currenciesMasterData;
    private Map<String, String> tenantIdsData;
    private String entryRefNo;
    private ShipmentDetailsQuoteDateType quoteDateType;
    private List<PartiesResponse> shipmentAddresses;
    private String flightStatus;
    private String fmcTlcId;
    private String commodity;
    private Long orderNumber;
    private String orderManagementId;
    private String orderManagementNumber;
    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
    private Map<String, String> textData;
    private Map<String, Long> containerData;
    private CustomerCategoryRates customerCategory;
    private UUID sourceGuid;
    private UUID clonedGuid;
    private List<String> implicationList;
    private Map<String, Object> masterDataMap;
    private Map<String, String> organizationsMasterData;
    private DateBehaviorType dateType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentGateInDate;
    private ShipmentPackStatus shipmentPackStatus;
    private OceanDGStatus oceanDGStatus;
    private Boolean syncRoutingFromConsolidation;
    private Boolean isNetworkFile;
    private Boolean b2b;
    private Long parentTenantId;
    private Boolean isCoLoadEnabled;
    private String issuingCarrierName;
    private String oceanBlNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadinessDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime brokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime brokerageAtDestinationDate;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime quoteDate;

    private MigrationStatus migrationStatus;
    private Boolean triggerMigrationWarning;
    private LocalDateTime carrierDocCutOff;
    private LocalDateTime cargoReceiptWHCutOff;
    private LocalDateTime lastFreeDateCutOff;
    private Integer numberOfFreeDaysCutOff;

    public void addTextData(Map<String, String> dataMap) {
        if(textData == null) {
            textData = new HashMap<>();
        }
        textData.putAll(dataMap);
    }
}
