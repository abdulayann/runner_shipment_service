package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class VerifiedGrossMassService implements IVerifiedGrossMassService {
    private final IVerifiedGrossMassDao verifiedGrossMassDao;
    private final JsonHelper jsonHelper;

    public VerifiedGrossMassService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper) {
        this.verifiedGrossMassDao = verifiedGrossMassDao;
        this.jsonHelper = jsonHelper;
    }

    @Override
    public VerifiedGrossMassResponse create(VerifiedGrossMassRequest request) {
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    @Override
    public VerifiedGrossMassResponse retrieveById(Long id) {
        Optional<VerifiedGrossMass> verifiedGrossMass = verifiedGrossMassDao.findById(id);
        if (verifiedGrossMass.isEmpty()) {
            throw new ValidationException("Invalid vgm id");
        }
        return jsonHelper.convertValue(verifiedGrossMass.get(), VerifiedGrossMassResponse.class);
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_NULL_ERROR);
        }
        if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
            throw new ValidationException(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE);
        }

        Pair<Specification<VerifiedGrossMass>, Pageable> tuple = fetchData(listCommonRequest, VerifiedGrossMass.class, VerifiedGrossMassConstants.tableNames);
        Page<VerifiedGrossMass> verifiedGrossMassPage = verifiedGrossMassDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(verifiedGrossMassPage.getContent(), getMasterData, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()));

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                verifiedGrossMassPage.getTotalPages(),
                verifiedGrossMassPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<VerifiedGrossMass> verifiedGrossMassList, boolean getMasterData,
                                                             Set<String> includeColumns) {
        List<VerifiedGrossMassListResponse> verifiedGrossMassListResponses = new ArrayList<>();

        for (VerifiedGrossMass verifiedGrossMass : verifiedGrossMassList) {
            VerifiedGrossMassListResponse verifiedGrossMassListResponse = jsonHelper.convertValue(verifiedGrossMass, VerifiedGrossMassListResponse.class);
            verifiedGrossMassListResponses.add(verifiedGrossMassListResponse);
        }

        List<IRunnerResponse> responseList = new ArrayList<>(verifiedGrossMassListResponses);
        return responseList;
    }

    @Override
    public VerifiedGrossMassResponse update(VerifiedGrossMassRequest request) {
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    @Override
    public void delete(Long id) {
        verifiedGrossMassDao.delete(id);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(Long vgmId) {
        return null;
    }
}

