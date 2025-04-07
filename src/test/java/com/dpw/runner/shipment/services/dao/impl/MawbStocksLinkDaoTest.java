package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbStocksLinkRepository;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class MawbStocksLinkDaoTest {

    @InjectMocks
    private MawbStocksLinkDao mawbStocksLinkDao;

    @Mock
    private IMawbStocksLinkRepository mawbStocksLinkRepository;
    @Mock
    private MawbStocksDao mawbStocksDao;

    @Test
    void save() {
        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().build();
        when(mawbStocksLinkRepository.save(any())).thenReturn(mawbStocksLink);
        assertEquals(mawbStocksLink, mawbStocksLinkDao.save(mawbStocksLink));
    }

    @Test
    void findAll() {
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listReq, Validations.class);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        assertEquals(mawbStocksLinkPage, mawbStocksLinkDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findByMawbNumber() {
        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().build();
        List<MawbStocksLink> links = Arrays.asList(mawbStocksLink);

        when(mawbStocksLinkRepository.findByMawbNumber(any())).thenReturn(links);
        assertEquals(links, mawbStocksLinkDao.findByMawbNumber(""));
    }

    @Test
    void deleteByParentId() {
        mawbStocksLinkDao.deleteByParentId(1L);
        verify(mawbStocksLinkRepository, times(1)).deleteByParentId(anyLong());
    }

    @Test
    void deLinkExistingMawbStockLink() {
        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().parentId(1L).build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);
        when(mawbStocksDao.findById(any())).thenReturn(Optional.of(MawbStocks.builder().availableCount("1").build()));

        mawbStocksLinkDao.deLinkExistingMawbStockLink("1");
        verify(mawbStocksLinkRepository, times(1)).save(any());
    }

    @Test
    void assignNextMawbNumber() {
        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().parentId(1L).mawbNumber("1234").build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        assertEquals("1234", mawbStocksLinkDao.assignNextMawbNumber(1L));
    }

    @Test
    void validateDuplicateMawbNumber() {
        MawbStocksLink mawbStocksLink = MawbStocksLink.builder().parentId(1L).mawbNumber("1234").build();
        List<MawbStocksLink> mawbStocksLinkList = new ArrayList<>();
        mawbStocksLinkList.add(mawbStocksLink);

        PageImpl<MawbStocksLink> mawbStocksLinkPage = new PageImpl<>(mawbStocksLinkList);
        when(mawbStocksLinkRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbStocksLinkPage);

        assertEquals(1, mawbStocksLinkDao.validateDuplicateMawbNumber(Collections.emptyList()));
    }
}