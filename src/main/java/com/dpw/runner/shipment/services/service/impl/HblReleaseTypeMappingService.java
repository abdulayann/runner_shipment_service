package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IHblReleaseTypeMappingDao;
import com.dpw.runner.shipment.services.dto.request.HblReleaseTypeMappingListRequest;
import com.dpw.runner.shipment.services.dto.response.HblReleaseTypeMappingResponse;
import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblReleaseTypeMappingService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Slf4j
@Service
public class HblReleaseTypeMappingService implements IHblReleaseTypeMappingService {

    @Autowired
    private IHblReleaseTypeMappingDao hblReleaseTypeMappingDao;

    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }


    @Override
    public ResponseEntity<?> retrieveByHblIdAndReleaseType(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            HblReleaseTypeMappingListRequest request = (HblReleaseTypeMappingListRequest) commonRequestModel.getData();
            if (request == null || request.getHblId() == null || request.getReleaseType() == null) {
                log.error("Request is empty for Hbl Release Type retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException("Invalid request for fetching original printed, make sure mandatory fields are available: HBL Id & Release Type");
            }

            List<HblReleaseTypeMapping> releaseTypeMappingList = hblReleaseTypeMappingDao.findByReleaseTypeAndHblId(request.getHblId(), request.getReleaseType());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(releaseTypeMappingList));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }


    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<HblReleaseTypeMapping> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(element -> {
            HblReleaseTypeMappingResponse response = modelMapper.map(element, HblReleaseTypeMappingResponse.class);
            responseList.add(response);
        });
        return responseList;
    }
}

