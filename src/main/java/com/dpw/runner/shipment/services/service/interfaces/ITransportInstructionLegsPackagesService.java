package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.lang.reflect.InvocationTargetException;


public interface ITransportInstructionLegsPackagesService {
    TransportInstructionLegsPackagesResponse create(TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsPackagesResponse update(TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsPackagesListResponse list(ListCommonRequest request, boolean getMasterData);

    TransportInstructionLegsPackagesResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsPackagesResponse retrieveById(Long id);

    TransportInstructionLegsPackagesListResponse bulkCreate(TransportInstructionLegsPackagesListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;
}
