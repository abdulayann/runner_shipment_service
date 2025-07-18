package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.lang.reflect.InvocationTargetException;


public interface ITransportInstructionLegsReferenceService {
    TransportInstructionLegsReferenceResponse create(TransportInstructionLegsReferenceRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsReferenceResponse update(TransportInstructionLegsReferenceRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsReferenceListResponse list(ListCommonRequest request, boolean getMasterData);

    TransportInstructionLegsReferenceResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsReferenceResponse retrieveById(Long id);

    TransportInstructionLegsReferenceListResponse bulkCreate(TransportInstructionLegsReferenceListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;
}
