package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IQuoteContractsDao;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.QuoteContractsResponse;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Service
@Slf4j
public class QuoteContractsService implements IQuoteContractsService {

    @Autowired
    private IQuoteContractsDao quoteContractsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return null;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Quote Contracts list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<QuoteContracts>, Pageable> tuple = fetchData(request, QuoteContracts.class);
            Page<QuoteContracts> quoteContractsPage = quoteContractsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Quote Contracts list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(quoteContractsPage.getContent()),
                    quoteContractsPage.getTotalPages(),
                    quoteContractsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    public void updateQuoteContracts(ListContractResponse request) {
        try {
            if(request == null) {
                log.error("Request is empty for Quote Contracts update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return;
            }
            String contractId = null;
            try {
                contractId = request.getContracts().get(0).getContract_id();
            } catch (Exception e) {
                log.error("Contract Id is null for Quote Contracts update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(IsStringNullOrEmpty(contractId)) {
                log.error("Contract Id is null for Quote Contracts update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return;
            }
            QuoteContracts quoteContracts;
            List<QuoteContracts> quoteContractsList = quoteContractsDao.findByContractId(contractId);
            if(quoteContractsList != null && !quoteContractsList.isEmpty()) {
                quoteContracts = quoteContractsList.get(0);
            } else {
                quoteContracts = new QuoteContracts();
                quoteContracts.setContractId(contractId);
            }
            getQuoteContractsData(quoteContracts, request, contractId);
            quoteContractsDao.save(quoteContracts);
        } catch (Exception e) {
            log.error("Error while updating quote Contracts with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
    }

    private void getQuoteContractsData(QuoteContracts quoteContracts, ListContractResponse request, String contractId) {
        quoteContracts.setContainerTypes(getContainerTypes(request, contractId));
    }

    private List<String> getContainerTypes(ListContractResponse request, String contractId) {
        List<String> containerTypes = new ArrayList<>();
        try {
            if(!Objects.isNull(request.getContracts().get(0).getContract_usage())) {
                for (ListContractResponse.ContractUsage contractUsage : request.getContracts().get(0).getContract_usage()) {
                    if(!Objects.isNull(contractUsage) && !Objects.isNull(contractUsage.getFilter_params()) &&
                            !Objects.isNull(contractUsage.getFilter_params().getCargo_type()))
                        containerTypes.addAll(contractUsage.getFilter_params().getCargo_type());
                }
            }
        } catch (Exception e) {
            log.info("Container Types not available for contract {}", contractId);
        }
        containerTypes.remove(NPMConstants.ANY);
        containerTypes.remove(null);
        return containerTypes;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<QuoteContracts> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(quoteContracts -> responseList.add(convertEntityToDto(quoteContracts)));
        return responseList;
    }

    private IRunnerResponse convertEntityToDto(QuoteContracts quoteContracts) {
        return jsonHelper.convertValue(quoteContracts, QuoteContractsResponse.class);
    }

    @Override
    public QuoteContracts getQuoteContractsByContractId(String contractId) {
        if(StringUtils.isEmpty(contractId)) {
            return null;
        }
        List<QuoteContracts> quoteContractsList = quoteContractsDao.findByContractId(contractId);
        if(!CommonUtils.listIsNullOrEmpty(quoteContractsList)) {
            return quoteContractsList.get(0);
        }
        return null;
    }

}
