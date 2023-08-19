package com.dpw.runner.shipment.services.dto.request.booking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;


@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BookingOffersRequest extends CommonRequest implements IRunnerRequest {
    @JsonProperty("request_offers")
    private List<OfferRequest> requestOffers;
}
