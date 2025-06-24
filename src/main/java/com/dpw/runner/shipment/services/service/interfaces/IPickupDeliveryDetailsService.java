package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Optional;


public interface IPickupDeliveryDetailsService extends ICommonService {
    ResponseEntity<IRunnerResponse> createV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateV2(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> deleteV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveByIdV2(CommonRequestModel commonRequestModel, boolean populateRAKC);

    Optional<PickupDeliveryDetails> findById(Long parentEntityId);

    List<PickupDeliveryDetails> findByShipmentId(Long shipmentId);

    void processDownStreamConsumerData(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId, PushToDownstreamEventDto message, String transactionId);
}
