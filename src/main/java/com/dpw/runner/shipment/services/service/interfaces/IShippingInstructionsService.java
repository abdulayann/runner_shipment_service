package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;

public interface IShippingInstructionsService {

    ShippingInstructionResponse createShippingInstruction(CommonRequestModel request);

    ShippingInstructionResponse getShippingInstructionsById(CommonGetRequest commonRequestModel);

    ShippingInstructionResponse updateShippingInstructions(Long id, CommonRequestModel request);

    void deleteShippingInstructions(Long id);
}
