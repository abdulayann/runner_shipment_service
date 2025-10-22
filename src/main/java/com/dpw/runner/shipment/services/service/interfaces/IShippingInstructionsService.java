package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.kafka.dto.inttra.ShippingInstructionEventDto;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

public interface IShippingInstructionsService {

    ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest request);

    ShippingInstructionResponse getShippingInstructionsById(Long id);

    ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest updatedInfo);

    void deleteShippingInstructions(Long id);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData);

    ResponseEntity<IRunnerResponse> getAllMasterData(Long shippingInstId);

    Page<ShippingInstruction> getShippingInstructions(ListCommonRequest listCommonRequest);

    ShippingInstructionResponse getDefaultShippingInstructionValues(EntityType type, Long entityId);

    ShippingInstructionResponse submitShippingInstruction(Long shippingInstId);

    ShippingInstructionResponse amendShippingInstruction(Long shippingInstId);

    void updateShippingInstructionsStatus(ShippingInstructionEventDto shippingInstruction, String fileName);
    void cancelShippingInstruction(Long id);

}
