package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MawbStocksConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dto.request.MawbStocksRequest;
import com.dpw.runner.shipment.services.dto.response.MawbStocksResponse;
import com.dpw.runner.shipment.services.dto.response.NextMawbCarrierResponse;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IMawbStocksService;
import com.dpw.runner.shipment.services.syncing.Entity.MawbStocksV2;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IMawbStockSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Slf4j
@Service
public class MawbStocksService implements IMawbStocksService {
    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    IMawbStockSync mawbStockSync;

    @Autowired
    SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        MawbStocksRequest request = null;
        request = (MawbStocksRequest) commonRequestModel.getData();
        request.setAvailableCount(request.getCount());
        if(request.getFrom() != null){
            request.setPrefix(request.getFrom().split("-")[0]);
        }
        request.setNextMawbNumber(request.getFrom());
        if (request == null) {
            log.debug("Request is empty for MAWB stocks create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        MawbStocks mawbStocks = convertRequestToEntity(request);

        try {
           mawbStocks = mawbStocksDao.save(mawbStocks);
           request.setId(mawbStocks.getId());
           var stockLinks = this.mawbStocksLinkBulkUpdate(request);
           mawbStocks.setMawbStocksLinkRows(stockLinks);
           log.info("MAWB stocks created successfully for Id {} with Request Id {}", mawbStocks.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        callV1Sync(mawbStocks);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(mawbStocks));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        MawbStocksRequest request = (MawbStocksRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Stocks update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for MAWB Stocks update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<MawbStocks> oldEntity = mawbStocksDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(MawbStocksConstants.MAWB_STOCKS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        MawbStocks mawbStocks = convertRequestToEntity(request);
        try {
            mawbStocks = mawbStocksDao.save(mawbStocks);
            log.info("Updated the MAWB stocks for Id {} with Requestr Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        callV1Sync(mawbStocks);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(mawbStocks));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for MAWB stocks list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<MawbStocks>, Pageable> tuple = fetchData(request, MawbStocks.class);
            Page<MawbStocks> mawbStocksPage = mawbStocksDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("MAWB stocks list retrieved successfully for RequestId {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(mawbStocksPage.getContent()),
                    mawbStocksPage.getTotalPages(),
                    mawbStocksPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for MAWB stocks async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<MawbStocks>, Pageable> tuple = fetchData(request, MawbStocks.class);
            Page<MawbStocks> mawbStocksPage = mawbStocksDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("MAWB stocks async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(mawbStocksPage.getContent()),
                    mawbStocksPage.getTotalPages(),
                    mawbStocksPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for MAWB stocks delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for MAWB Stocks delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(id);
            if (!mawbStocks.isPresent()) {
                log.debug(MawbStocksConstants.MAWB_STOCKS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            mawbStocksDao.delete(mawbStocks.get());
            log.info("Deleted MAWB Stocks for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for MAWB Stocks retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for MAWB Stocks retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(id);
            if (!mawbStocks.isPresent()) {
                log.debug(MawbStocksConstants.MAWB_STOCKS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("MAWB Stocks details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            MawbStocksResponse response = convertEntityToDto(mawbStocks.get());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> getNextMawbNumberByCarrier(String airLinePrefix, String borrowedFrom){
        ListCommonRequest listCommonRequest;
        if (StringUtility.isEmpty(borrowedFrom))
            listCommonRequest = CommonUtils.andCriteria("borrowedFrom", null, "ISNULL", null);
        else
            listCommonRequest = CommonUtils.andCriteria("borrowedFrom", borrowedFrom, "=", null);
        CommonUtils.andCriteria("id", 0, ">", listCommonRequest);
        CommonUtils.andCriteria("consolidationId", null, "ISNULL", listCommonRequest);
        CommonUtils.andCriteria("availableCount", "0", ">", listCommonRequest);
        CommonUtils.andCriteria("airLinePrefix", airLinePrefix.toLowerCase(), "=", listCommonRequest);

        Pair<Specification<MawbStocks>, Pageable> tuple = fetchData(listCommonRequest, MawbStocks.class);
        Page<MawbStocks> mawbStocksPage = mawbStocksDao.findAll(tuple.getLeft(), tuple.getRight());
        if(!mawbStocksPage.getContent().isEmpty()){
            return ResponseHelper.buildSuccessResponse(NextMawbCarrierResponse.builder().nextMawbNumber(mawbStocksPage.getContent().get(0).getNextMawbNumber()).build());
        }
        return ResponseHelper.buildSuccessResponse(NextMawbCarrierResponse.builder().nextMawbNumber(null).build());
    }

    private List<MawbStocksLink> mawbStocksLinkBulkUpdate(MawbStocksRequest mawbStocksRequest) throws ValidationException {
        int count =  Integer.parseInt(mawbStocksRequest.getAvailableCount());
        List<MawbStocksLink> requestlist = new ArrayList<>();

        List<String> nums = new ArrayList<>();
        String startingNum = mawbStocksRequest.getMawbNumber();
        for (int i = 0; i < count; i++) {
            int val = (Integer.parseInt(startingNum) + i) % 7;
            String stNum = Integer.parseInt(startingNum) + i + "" + val;
            int appendLeadingZeros = 8 - stNum.length();
            for (int ind = 0; ind < appendLeadingZeros; ind++) {
                stNum = "0" + stNum;
            }
            nums.add(stNum);
        }
        List<String> mawbNumbers = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            MawbStocksLink mawbStocksLink = new MawbStocksLink();
            mawbStocksLink.setParentId(mawbStocksRequest.getId());
            mawbStocksLink.setSeqNumber(Long.toString(Long.parseLong(mawbStocksRequest.getMawbNumber())  + i));
            var leadingZerosForSeqNumber = 7 - mawbStocksLink.getSeqNumber().length();
            for(int itr = 0; itr<leadingZerosForSeqNumber;itr++){
                mawbStocksLink.setSeqNumber("0" + mawbStocksLink.getSeqNumber());
            }
            mawbStocksLink.setMawbNumber((mawbStocksRequest.getPrefix() + "-" + nums.get(i)));
            mawbNumbers.add(mawbStocksLink.getMawbNumber());
            mawbStocksLink.setStatus("Unused");
            requestlist.add(mawbStocksLink);
        }

        beforeSave(mawbStocksRequest, mawbNumbers);

        for (MawbStocksLink request:requestlist) {
            mawbStocksLinkDao.save(request);
        }

        return requestlist;
    }

    private MawbStocksResponse convertEntityToDto(MawbStocks mawbStocks) {
        return jsonHelper.convertValue(mawbStocks, MawbStocksResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<MawbStocks> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(mawbStocks -> responseList.add(convertEntityToDto(mawbStocks)));
        return responseList;
    }

    private MawbStocks convertRequestToEntity(MawbStocksRequest request) {
        return jsonHelper.convertValue(request, MawbStocks.class);
    }

    private void callV1Sync(MawbStocks mawbStocks) {
        try {
            mawbStockSync.sync(mawbStocks);
        }
        catch (Exception error) {
            log.error("error while performing syncing MAWB_STOCKS : ",  error.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> createV1MawbStocks(CommonRequestModel commonRequestModel, Boolean checkForSync) throws RunnerException {
        MawbStocks mawbStocks = null;
        try {
            MawbStocksV2 mawbStocksV2 = (MawbStocksV2) commonRequestModel.getData();
            Optional<MawbStocks> optional = mawbStocksDao.findByGuid(mawbStocksV2.getGuid());
            mawbStocks = syncEntityConversionService.mawbStocksV1ToV2(mawbStocksV2);
            if(optional.isPresent()) {
                var mawbStockId = optional.get().getId();
                mawbStocks.setId(mawbStockId);
                mawbStocksLinkDao.deleteByParentId(mawbStockId);
            }
            List<MawbStocksLink> mawbStocksLinks = mawbStocks.getMawbStocksLinkRows() != null ? mawbStocks.getMawbStocksLinkRows() : new ArrayList<>();
            mawbStocksDao.save(mawbStocks);
            for(var stockLink : mawbStocksLinks) {
                stockLink.setParentId(mawbStocks.getId());
                mawbStocksLinkDao.save(stockLink);
            }
        }
        catch(Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return (ResponseEntity<IRunnerResponse>) ResponseHelper.buildSuccessResponse(convertEntityToDto(mawbStocks));
    }

    private void beforeSave(MawbStocksRequest mawbStocks, List<String> mawbNumbers) throws ValidationException {
        if(isValidMawb(mawbStocks.getFrom()) && isValidMawb(mawbStocks.getTo())) {
            Long count = mawbStocksLinkDao.validateDuplicateMawbNumber(mawbNumbers);
            if(count > 0) {
                throw new ValidationException(MawbStocksConstants.DUPLICATE_MAWB_STOCK_VALIDATION);
            }
        }
    }

    private boolean isValidMawb(String mawb) {
        if(!StringUtility.isEmpty(mawb) && mawb.contains("-") && (mawb.length() - mawb.indexOf("-") == 9))
            return true;
        return false;
    }

//    private void setManyToOneRelationships(MawbStocks mawbStocks){
//        if(mawbStocks.getMawbStocksLinkRows() !=null){
//            List<MawbStocksLink> mawbStocksLinks = mawbStocks.getMawbStocksLinkRows();
//            for (MawbStocksLink mawbStocksLink: mawbStocksLinks) {
//                mawbStocksLink.setMawbStocks(mawbStocks);
//            }
//        }
//    }
}
