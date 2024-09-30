package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.CommonRequest;
import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class JobRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private Long consolidationId;
    private PartiesRequest buyerDetail;
    private PartiesRequest supplierDetail;
    private String orderNumber;
    private LocalDateTime orderDate;
    private String confirmNumber;
    private LocalDateTime confirmDate;
    private String invoiceNumber;
    private LocalDateTime invoiceDate;
    private Long buyerId;
    private String orderStatus;
    private LocalDateTime followUpDate;
    private String description;
    private String currency;
    private String serviceMode;
    private String incoTerm;
    private String additionalTerms;
    private String transportMode;
    private String countryOfOrigin;
    private List<EventsRequest> eventsList;
}
