package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CustomerBookingResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String serviceMode;
    private BookingStatus bookingStatus;
    private PartiesResponse customer;
    private Boolean isCustomerFreeText;
    private PartiesResponse consignor;
    private Boolean isConsignorFreeText;
    private PartiesResponse consignee;
    private Boolean isConsigneeFreeText;
    private PartiesResponse notifyParty;
    private Boolean isNotifyPartyFreeText;
    private String customerEmail;
    private String bookingNumber;
    private LocalDateTime bookingDate;
    private String incoTerms;
    private CarrierDetailResponse carrierDetails;
    private String transportType;
    private String cargoType;
    private String direction;
    private Integer quantity;
    private String quantityUnit;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private String contractId;
    private String createdBy;
    private String contractStatus;
    private BookingSource source;
    private String businessCode;
    private String shipmentId;
    private List<ContainerResponse> containersList;
    private List<PackingResponse> packingList;
    private List<RoutingsResponse> routingList;
    private List<BookingChargesResponse> bookingCharges;
    private List<FileRepoResponse> fileRepoList;
    public Map<String, String> masterData;
    private Boolean isAutoWeightVolumeUpdate;
    private String fmcTlcId;
    private BigDecimal totalRevenue;
    private Boolean isPackageManual;
    private Boolean isConsignorAddressFreeText;
    private Boolean isConsigneeAddressFreeText;
    private Boolean isCustomerAddressFreeText;
    private Boolean isNotifyPartyAddressFreeText;
}
