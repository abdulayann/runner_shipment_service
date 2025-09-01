package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;

public interface IShippingInstructionsService {

    ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest request);

    ShippingInstructionResponse getShippingInstructionsById(Long id);

    ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest updatedInfo);

    void deleteShippingInstructions(Long id);
}
