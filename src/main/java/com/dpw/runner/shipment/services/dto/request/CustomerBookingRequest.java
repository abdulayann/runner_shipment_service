package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CustomerBookingRequest  extends CommonRequest implements IRunnerRequest {
    private Long id;
    private BookingStatus bookingStatus;
    private PartiesRequest customer;
    private Boolean isCustomerFreeText;
    private PartiesRequest consignor;
    private Boolean isConsignorFreeText;
    private PartiesRequest consignee;
    private Boolean isConsigneeFreeText;
    private PartiesRequest notifyParty;
    private Boolean isNotifyPartyFreeText;
    private String customerEmail;
    private String bookingNumber;
    private LocalDateTime bookingDate;
    private String incoTerms;
    private CarrierDetailRequest carrierDetails;
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
    private String contractStatus;
    private BookingSource source;
    private List<ContainerRequest> containersList;
    private List<PackingRequest> packingList;
    private List<RoutingsRequest> routingList;
    private List<BookingChargesRequest> bookingCharges;
    private List<FileRepoRequest> fileRepoList;
}
