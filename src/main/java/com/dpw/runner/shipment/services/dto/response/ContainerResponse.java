package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Setter
@ApiModel("Container Response Model")
public class ContainerResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private Long bookingId;
    private Long loggingId;
    private String containerCode;
    private String containerNumber;
    private String sealNumber;
    private String descriptionOfGoods;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    private String netWeightUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal measurement;
    private String measurementUnit;
    private String commodityCode;
    private String hsCode;
    private Boolean isShipperOwned;
    private Boolean isEmpty;
    private Long containerCount;
    private String carrierSealNumber;
    private String shipperSealNumber;
    private String terminalOperatorSealNumber;
    private String veterinarySealNumber;
    private String customsSealNumber;
    private String customsReleaseCode;
    private String containerStuffingLocation;
    private String containerComments;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal grossVolume;
    private String grossVolumeUnit;
    private Boolean isReefer;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    private String hblDeliveryMode;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime allocationDate;
    private String dgClass;
    private Boolean hazardous;
    private String hazardousUn;
    private BigDecimal tareWeight;
    private String tareWeightUnit;
    private String serialNumber;
    private String innerPackageNumber;
    private String innerPackageType;
    private BigDecimal packageLength;
    private BigDecimal packageBreadth;
    private BigDecimal packageHeight;
    private Boolean isTemperatureMaintained;
    private String packs;
    private String packsType;
    private String marksNums;
    private String innerPackageMeasurementUnit;
    private String pacrNumber;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargeable;
    private String chargeableUnit;
    private Boolean isOwnContainer;
    private String transportMode;
    private ContainerStatus status;
    private String extraParams;
    private String remarks;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal allocatedWeight;
    private String allocatedWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal allocatedVolume;
    private String allocatedVolumeUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal achievedWeight;
    private String achievedWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal achievedVolume;
    private String achievedVolumeUnit;
    private String weightUtilization;
    private String volumeUtilization;
    private String commodityGroup;
    private Boolean isContractEnforced;
    private PartiesResponse pickupAddress;
    private PartiesResponse deliveryAddress;
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> commodityTypeData;
    private Map<String, String> containerCodeData;
    private Long contractEnforcedQuantityLimit;
    private String ownType;
    private String handlingInfo;
    private Map<String, String> textFieldData;
    private Boolean isPart;
    private Boolean isAttached;
//    private List<TruckDriverDetailsResponse> truckingDetails;
    private List<EventsResponse> eventsList;
    private String hblNumber;
    private String invoiceNumber;
    private String invoiceCurrency;
    private BigDecimal invoiceValue;
    private Integer tenantId;
    private String unNumber;
    private String properShippingName;
    private String packingGroup;
    private BigDecimal minimumFlashPoint;
    private String minimumFlashPointUnit;
    private Boolean marinePollutant;
    private ContainerPraStatus praStatus;
}
