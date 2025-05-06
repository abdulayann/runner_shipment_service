package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.entity.AppConfig;
import java.io.Serializable;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IAppRepository extends JpaRepository<AppConfig, Serializable> {

  Optional<AppConfig> findByKey(String key);
}
