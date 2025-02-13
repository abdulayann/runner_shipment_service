package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbStocksLinkRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class MawbStocksLinkDao implements IMawbStocksLinkDao {
    @Autowired
    private IMawbStocksLinkRepository mawbStocksLinkRepository;
    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Override
    public MawbStocksLink save(MawbStocksLink mawbStocksLink) {
        return mawbStocksLinkRepository.save(mawbStocksLink);
    }

    @Override
    public Page<MawbStocksLink> findAll(Specification<MawbStocksLink> spec, Pageable pageable) {
        return mawbStocksLinkRepository.findAll(spec, pageable);
    }

    @Override
    public List<MawbStocksLink> findByMawbNumber(String mawbNumber) {
        return mawbStocksLinkRepository.findByMawbNumber(mawbNumber);
    }

    @Override
    @Transactional
    public void deleteByParentId(Long parentId) {
        mawbStocksLinkRepository.deleteByParentId(parentId);
    }

    @Override
    public void deLinkExistingMawbStockLink(String mawbNumber) {
        if (mawbNumber == null)
            return;
        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", mawbNumber, "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        if (!mawbStocksLinkPage.isEmpty() && mawbStocksLinkPage.getTotalElements() > 0) {
            MawbStocksLink oldMawbStocksLink = mawbStocksLinkPage.getContent().get(0);
            oldMawbStocksLink.setStatus(Constants.UNUSED);
            oldMawbStocksLink.setEntityId(null);
            oldMawbStocksLink.setEntityType(null);
            oldMawbStocksLink.setShipConsNumber(null);
            save(oldMawbStocksLink);

            deLinkMawbAvailableCount(oldMawbStocksLink.getParentId());
        }
    }

    private void deLinkMawbAvailableCount(Long parentId) {
        Optional<MawbStocks> optional = mawbStocksDao.findById(parentId);
        if (optional.isPresent()) {
            var mawbStock = optional.get();
            Long nextCount = Long.parseLong(mawbStock.getAvailableCount()) + 1;
            mawbStock.setAvailableCount(nextCount.toString());
            mawbStock.setNextMawbNumber(assignNextMawbNumber(parentId));

            mawbStocksDao.save(mawbStock);
        }
    }


    @Override
    public String assignNextMawbNumber(Long parentId) {
        ListCommonRequest listCommonRequest;
        listCommonRequest = CommonUtils.andCriteria("parentId", parentId, "=", null);
        CommonUtils.andCriteria("status", Constants.UNUSED, "=", listCommonRequest);
        listCommonRequest.setSortRequest(SortRequest.builder()
                .fieldName("seqNumber")
                .order("DESC")
                .build());
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listCommonRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinks = findAll(pair.getLeft(), pair.getRight());
        if (!mawbStocksLinks.isEmpty()) {
            return mawbStocksLinks.get().toList().get(0).getMawbNumber();
        }
        return null;
    }

    @Override
    public Long validateDuplicateMawbNumber(List<String> mawbNumbers) {
        ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("mawbNumber", mawbNumbers, "IN");
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listCommonRequest, MawbStocksLink.class);
        Page<MawbStocksLink> page = findAll(pair.getLeft(), pair.getRight());
        return page.getTotalElements();
    }

}
